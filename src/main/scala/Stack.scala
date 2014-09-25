package stack

import org.json4s._
import org.json4s.native.JsonMethods._
import java.net.URL
import java.io.File

object Service {

  /** https://docs.docker.com/reference/builder/ */
  object Def {
    def fromJson(js: JValue): Option[Def] = (for {
      JObject(df)               <- js
      ("image", JString(image)) <- df
    } yield {
      def str(name: String) = (for {
        (`name`, JString(value)) <- df 
      } yield value).headOption

      def int(name: String) = (for {
        (`name`, JInt(value)) <- df    
      } yield value.toInt).headOption

      def bool(name: String) = (for {
        (`name`, JBool(value)) <- df
      } yield value).headOption

      def list(name: String) = for {
        (`name`, JArray(xs)) <- df
        JString(value)       <- xs
      } yield value

      Def(
        image,
        str("cmd"),
        list("links"),
        list("ports"),
        list("expose"),
        list("volumes"),
        list("volumes_from"),
        (for {
          ("env", JObject(env)) <- df
          (key, JString(value)) <- env
        } yield (key, value)).toMap,
        list("dns"),
        str("working_dir"),
        str("entrypoint"),
        str("user"),
        str("hostname"),
        bool("privledged"),
        int("memorylimit")
      )
    }).headOption
  }
  case class Def(
    image: String,
    cmd: Option[String]      = None,
    links: Seq[String]       = Seq.empty,
    ports: Seq[String]       = Seq.empty,
    expose: Seq[String]      = Seq.empty,
    volumes: Seq[String]     = Seq.empty,
    volumesFrom: Seq[String] = Seq.empty,
    env: Map[String, String] = Map.empty,
    dns: Seq[String]         = Seq.empty,
    workingDir: Option[String]  = None,
    entrypoint: Option[String]  = None,
    user: Option[String]        = None,
    hostname: Option[String]    = None,
    privileged: Option[Boolean] = None,
    memoryLimit: Option[Int]    = None)
}

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
