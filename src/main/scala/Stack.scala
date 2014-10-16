package stack

import tugboat.{ Docker, Pull }
import org.json4s._
import org.json4s.native.JsonMethods._
import java.net.URL
import java.io.File
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Success, Failure }

object Stack {
  val DefaultName =
    new File(System.getProperty("user.dir")).getName

  def fromUrl
   (docker: Docker, name: String = DefaultName)
   (url: URL): Either[String, Stack] =
    parseOpt(io.Source.fromURL(url).getLines.mkString)
      .map { js =>
        (for {
          JObject(stack) <- js
          ("services", JObject(services)) <- stack
          (name, svc)                     <- services
          df                              <- Service.Def.fromJson(svc)
        } yield (name, df)).toMap
      } match {
        case Some(defs) =>
          Right(Stack(name, defs, docker))
        case _ => Left("failed to load stack")
      }

  def fromFile
   (docker: Docker, name: String = DefaultName)
   (path: String): Either[String, Stack] =
    fromUrl(docker, name)(new File(path).toURL)
}

case class Stack
 (name: String, defs: Map[String, Service.Def], docker: Docker) {
  /** set of target container names */
  private[this] val names = defs.keySet.map(sname => s"${name}_$sname")
  
  private[this] val loggers: Map[String, Logger] = Logger.named(defs.keySet)

  private def promiseMap[T] =
    defs.map { case (name, _) => (name, Promise[T]()) }

  private def svcName(sname: String) =
    sname.replaceFirst(s"${name}_", "")

  /** list running stacked services */
  def ps
   (implicit ec: ExecutionContext): Future[Unit] = {
     docker.containers.list().map {
       case xs =>
         val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        running.foreach { c =>
          println(s"${svcName(c.names.head.drop(1))} ${c.id} ${c.image} ${c.status} ${c.ports.mkString(", ")}")
        }
     }
   }

  /** streams the logs for stacked services */
  def logs
   (implicit ec: ExecutionContext): Future[Unit] = {
    docker.containers.list().map {
      case xs =>
        val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        running.foreach { c =>
          val log = loggers(svcName(c.names.head.drop(1)))
          docker.containers.get(c.id)
            .logs.stdout(true).stderr(true)
            .follow(true).stream { str =>
              log.println(str)
            }
        }
    }
  }

  /** stops and deletes any containers for stacked services */
  def down
   (implicit ec: ExecutionContext): Map[String, Future[Unit]] = {
    val promises = promiseMap[Unit]
    docker.containers.list.all().map {
      case xs =>
        val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        // satisfy all service promises not running
        val runningNames = running.map( c => svcName(c.names.head.drop(1)) )
        promises.filterNot { case (name, _) => runningNames.contains(name) }.foreach {
          case (_, promise) => promise.success(())
        }
        // shutdown service containers
        running.map { c =>
          val name = svcName(c.names.head.drop(1))
          val log = loggers(name)
          val promise = promises(name)
          val proxy = docker.containers.get(c.id)
          def del = {
            log.println("deleting container")
            proxy.delete()
          }
          val takedown =
            if (c.status.startsWith("Exited")) del else {
              log.println("stopping container")
              proxy.stop()().flatMap {
                case _ => del
              }
            }
          takedown.onComplete {
            case Success(_) =>
              log.println("deleted container")
              promise.success(())
            case Failure(e) =>
              promise.failure(e)
          }
        }
    }
    promises.map { case (sname, promise) =>
      (sname, promise.future)
    }
  }

  /** builds and starts containers for stacked services */
  def up
   (implicit ec: ExecutionContext): Map[String, Future[String]] = {
    val promises = promiseMap[String]
    def sup(svc: (String, Service.Def)): Unit = svc match {
      case (sname, df) =>
        val log = loggers(sname)
        val promise = promises(sname)
        log.println(s"creating container image ${df.image}")
        docker.containers.create(df.image).name(s"${name}_$sname")()
          .onComplete {
            case Success(resp) =>
              log.println(s"starting container")
              docker.containers.get(resp.id).start()
                .onComplete {
                  case Success(_) =>
                    log.println(s"started container ${resp.id}")
                    promise.success(resp.id)
                  case Failure(f2) =>
                    log.println(s"failed to start container ${resp.id}: $f2")
                    promise.failure(f2)
                }
            case Failure(Docker.Error(404, _)) =>
              log.println(s"Unable to find image '${df.image}' locally")
              docker.images.pull(df.image).stream {
                case Pull.Status(msg) =>
                  log.println(msg)
                case Pull.Progress(msg, id, details) =>
                  if (msg.startsWith("Download complete")) System.out.println()
                  if (!msg.startsWith("Downloading")) log.println(s"$id : $msg")
                  details.foreach { dets =>
                    log.print(s"$id : $msg ${dets.bar}")
                  }
                case Pull.Error(msg, _) =>
                  log.println(msg)
              }.map {
                case _ =>
                  log.println("done making")
                  sup(svc)
              }
            case Failure(f) =>
              log.println(s"failed to create container from image ${df.image}: ${f.getMessage}")
              promise.failure(f)
          }
      }
     defs.map(sup)
     promises.map { case (sname, promise) =>
       (sname, promise.future)
     }
   }
}
