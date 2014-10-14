package stack

import org.scalatest.FunSpec
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class StackSpec extends FunSpec {
  describe("stack") {
    it ("should parse config files") {
      val tb = tugboat.Client()
      Stack.fromUrl(tb)(getClass().getResource("/stack.json")) match {
        case Right(stack) =>
          println(stack)
          Await.ready(stack.down, Duration.Inf)
          Await.ready(Future.sequence(stack.up.values), Duration.Inf)
        case Left(failed) =>
          fail(failed)
      }
      tb.close()
    }
  }
}
