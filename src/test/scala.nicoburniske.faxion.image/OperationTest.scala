package scala.nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import nicoburniske.faxion.image.Operation
import nicoburniske.faxion.Bottom
import nicoburniske.faxion.model.{Bottom, Top}
import org.scalatest.funsuite.AnyFunSuite

class OperationTest extends AnyFunSuite {
  val pantsPath = "example/fit1/pants.jpeg"
  val poloPath  = "example/fit1/polo.jpeg"

  test("Should categorize pants.jpeg as Bottom") {
    val pants = ImmutableImage.loader().fromFile(pantsPath)

    assert(Operation.classify(pants).get == Bottom)
  }
  test("Should stitch images together properly") {
    val pants = ImmutableImage.loader().fromFile(pantsPath)
    val polo  = ImmutableImage.loader().fromFile(poloPath)

    val fit         = Seq(polo, pants)
    val fitWithTags = Seq(polo -> Top, pants -> Bottom)

    val stitched         = Operation.stitchImages(fit)
    val stitchedWithTags = Operation.stitchImagesWithTags(fitWithTags)
    // TODO: how to program assertions? ensure pant + polo are contained in generated image?
  }

  test("Crop background") {
    // val pants   = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")
    val pantsGrailed = ImmutableImage.loader().fromFile("example/pantsGrailed.jpg")
    val cropped      = Operation.cropBackground(pantsGrailed)
    cropped.output(JpegWriter.Default, "cropped.jpeg")
  }

  test("OpenCV crop background") {
    val cropped = Operation.cropBackground(pantsPath)
    cropped.output(JpegWriter.Default, "cropped.jpeg")
  }

}
