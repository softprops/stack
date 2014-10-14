package stack

import tugboat.{ Client, Pull }
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
   (tb: Client, name: String = DefaultName)
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
          Right(Stack(name, defs, tb))
        case _ => Left("failed to load stack")
      }

  def fromFile
   (tb: Client, name: String = DefaultName)
   (path: String): Either[String, Stack] =
    fromUrl(tb, name)(new File(path).toURL)
}

case class Stack
 (name: String, defs: Map[String, Service.Def], tb: Client) {
  private[this] val names = defs.keySet.map(sname => s"${name}_$sname")
  private[this] val colors = Color.wheel
  private[this] val loggers = {
    val pad = defs.keys.map(_.size).max
    defs.map { case (name, _) =>
      val color = colors.next
      (name, (s: String) => System.out.synchronized {
        println(
          ("%s %0$" + pad + "s |\033[0m %s").format(color, name, s))
      })
    }
  }
  private def promiseMap[T] =
    defs.map { case (name, _) => (name, Promise[T]()) }

  private def svcName(sname: String) =
    sname.replaceFirst(s"${name}_", "")

  def logs
   (implicit ec: ExecutionContext) = {
    tb.containers.list().map {
      case xs =>
        val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        running.foreach { c =>
          val log = loggers(svcName(c.names.head.drop(1)))
          tb.containers.get(c.id)
            .logs.stdout(true).stderr(true).timestamps(true)
            .follow(true).stream { str =>
              log(str)
            }
        }
    }
  }

  def down
   (implicit ec: ExecutionContext): Future[List[(String, Future[Unit])]] = {
    tb.containers.list.all().map {
      case xs =>
        val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        running.map { c =>
          val name = svcName(c.names.head.drop(1))
          val log = loggers(name)
          val proxy = tb.containers.get(c.id)
          def del = {
            log("deleting")
            proxy.delete()
          }
          val takedown =
            if (c.status.startsWith("Exited")) del else {
              log("stopping")
              proxy.stop()().flatMap {
                case _ => del
              }
            }
          takedown.foreach { case _ => log("deleted container") }
          (name, takedown)
        }
    }
  }

  def up
   (implicit ec: ExecutionContext): Map[String, Future[String]] = {
    val promises = promiseMap[String]
    def make(svc: (String, Service.Def)): Unit = svc match {
      case (sname, df) =>
        val log = loggers(sname)
        val promise = promises(sname)
        log(s"creating container named $name from image ${df.image}")
        tb.containers.create(df.image).name(s"${name}_$sname")()
          .onComplete {
            case Success(resp) =>
              log(s"starting container ${resp.id}")
              tb.containers.get(resp.id).start()
                .onComplete {
                  case Success(_) =>
                    log(s"started container ${resp.id}")
                    promise.success(resp.id)
                  case Failure(f2) =>
                    log(s"failed to start container ${resp.id}: $f2")
                    promise.failure(f2)
                }
            case Failure(Client.Error(404, _)) =>
              log(s"Unable to find image '${df.image}' locally")
              tb.images.pull(df.image).stream {
                case Pull.Status(msg) =>
                  log(msg)
                case Pull.Progress(msg, _, details) =>
                  log(msg)
                  details.foreach { dets =>
                    log(dets.bar)
                  }
                case Pull.Error(msg, _) =>
                  log(msg)
              }.map {
                case _ =>
                  log("done making")
                  make(name, df)
              }
            case Failure(f) =>
              log(s"failed to create container from image ${df.image}: ${f.getMessage}")
              promise.failure(f)
          }
      }
     defs.map(make)
     promises.map { case (sname, p) => (sname, p.future) }
   }
}
