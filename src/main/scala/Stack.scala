package stack

import org.json4s._
import org.json4s.native.JsonMethods._
import java.net.URL
import java.io.File

object Stack {
  def fromUrl(url: URL): Either[String, Stack] =
    parseOpt(io.Source.fromURL(url).getLines.mkString)
      .map { js =>
        (for {
          JObject(services) <- js
          (name, svc)       <- services
        } yield {
          val df = Service.Def.fromJson(svc)
          println(df)
          (name, df.get)
        }
        ).toMap
      } match {
        case Some(defs) => Right(Stack(defs))
        case _ => Left("failed to load stack")
      }

  def fromFile(path: String): Either[String, Stack] = 
    fromUrl(new File(path).toURL)
}

case class Stack(defs: Map[String, Service.Def])
