package stack

import org.scalatest.FunSpec
import scala.concurrent.{ Future, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class StackSpec extends FunSpec {
  describe("stack") {
    it ("should parse config files") {
      Stack.fromUrl(getClass().getResource("/stack.json")) match {
        case Right(stack) =>
          println(stack)
          val tb = tugboat.Client()
          Await.ready(stack.down(tb), Duration.Inf)
          Await.ready(Future.sequence(stack.up(tb).values), Duration.Inf)
          tb.close()
        case Left(failed) =>
          fail(failed)
      }
    }
  }
}
