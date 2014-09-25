package stack

import org.scalatest.FunSpec

class StackSpec extends FunSpec {
  describe("stack") {
    it ("should parse config files") {
      Stack.fromUrl(getClass().getResource("/stack.json")) match {
        case Right(stack) =>
          println(stack)
        case Left(failed) => fail(failed)
      }
    }
  }
}
