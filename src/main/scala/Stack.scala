package stack

import tugboat.{ Client, Pull }
import org.json4s._
import org.json4s.native.JsonMethods._
import java.net.URL
import java.io.File
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Success, Failure }

object Stack {
  def fromUrl(url: URL): Either[String, Stack] =
    parseOpt(io.Source.fromURL(url).getLines.mkString)
      .map { js =>
        (for {
          JObject(stack) <- js
          ("services", JObject(services)) <- stack
          (name, svc)                     <- services
          df                              <- Service.Def.fromJson(svc)
        } yield (name, df)).toMap
      } match {
        case Some(defs) => Right(Stack(defs))
        case _ => Left("failed to load stack")
      }

  def fromFile(path: String): Either[String, Stack] = 
    fromUrl(new File(path).toURL)
}

case class Stack
 (defs: Map[String, Service.Def]) {
  private[this] val names = defs.keySet
  private[this] val colors = Color.wheel
  private[this] val loggers = {
    val pad = defs.keys.map(_.size).max
    defs.map { case (name, _) =>
      val color = colors.next
      (name, (s: String) => System.out.sycnronized {
        println(
          ("%s %0$" + pad + "s |\033[0m %s").format(color, name, s))
      })
    }
  }
  private def promiseMap[T] =
    defs.map { case (name, _) => (name, Promise[T]()) }

  def down(tb: Client)
   (implicit ec: ExecutionContext): Future[List[(String, Future[Unit])]] = {
    tb.containers.list.all().map {
      case xs =>
        val running = xs.filter(_.names.nonEmpty).filter { c =>
          c.names.exists( n => names.contains(n.drop(1)))
        }
        running.map { c =>
          val log = loggers(c.names.head.drop(1))
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
          (c.names.head, takedown)
        }
    }
  }

  def up(tb: Client)
   (implicit ec: ExecutionContext): Map[String, Future[String]] = {
    val promises = promiseMap[String]
    def make(svc: (String, Service.Def)): Unit = svc match {
      case (name, df) =>
        val log = loggers(name)
        log(s"creating container named $name from image ${df.image}")
        tb.containers.create(df.image).name(name)().onComplete {
          case Success(resp) =>
            log(s"starting container ${resp.id}")
            tb.containers.get(resp.id).start().onComplete {
              case Success(_) =>
                log(s"started container ${resp.id}")
                promises(name).success(resp.id)
              case Failure(f2) =>
                log(s"failed to start container ${resp.id}: $f2")
                promises(name).failure(f2)
            }
          case Failure(Client.Error(404, _)) =>
            log(s"Unable to find image '${df.image}' locally")
            tb.images.pull(df.image).stream {
              case Pull.Status(msg) => log(msg)
              case Pull.Progress(msg, _, details) =>
                log(msg)
                details.foreach { dets =>
                  log(dets.bar)
                }
              case Pull.Error(msg, _) => log(msg)
            }.map {
              case _ =>
                log("done making")
                make(name, df)
            }
          case Failure(f) =>
            log(s"failed to create container from image ${df.image}: ${f.getMessage}")
            promises(name).failure(f)
        }
    }
    defs.map(make)
    promises.map { case (name, p) => (name, p.future) }
  }
}
