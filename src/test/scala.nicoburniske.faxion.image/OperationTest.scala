package scala.nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.image.Operation
import nicoburniske.faxion.{Bottom, Top}
import org.scalatest.funsuite.AnyFunSuite

class OperationTest extends AnyFunSuite {
  test("Should categorize pants.jpeg as Bottom") {
    val pants = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")

    assert(Operation.classify(pants).get == Bottom)
  }
  test("Should stitch images together properly") {
    val pants = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")
    val polo  = ImmutableImage.loader().fromFile("example/fit1/polo.jpeg")

    val fit         = Seq(polo, pants)
    val fitWithTags = Seq(polo -> Top, pants -> Bottom)

    val stitched         = Operation.stitchImages(fit)
    val stitchedWithTags = Operation.stitchImagesWithTags(fitWithTags)
    // TODO: how to program assertions? ensure pant + polo are contained in generated image?
  }
}
