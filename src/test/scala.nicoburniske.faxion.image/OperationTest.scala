package scala.nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.Bottom
import nicoburniske.faxion.image.Operation
import org.scalatest.funsuite.AnyFunSuite

class OperationTest extends AnyFunSuite {
  test("Should categorize pants.jpeg as Bottom") {
    val pants = ImmutableImage.loader().fromFile("example/pants.jpeg")

    assert(Operation.classify(pants).get == Bottom)
  }
}
