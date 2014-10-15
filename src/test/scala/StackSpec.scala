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
          Await.ready(Future.sequence(stack.down.values), Duration.Inf)
          Await.ready(Future.sequence(stack.up.values), Duration.Inf)
          stack.logs
        case Left(failed) =>
          fail(failed)
      }
      tb.close()
    }
  }
}
