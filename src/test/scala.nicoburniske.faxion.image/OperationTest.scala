package scala.nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import nicoburniske.faxion.image.Operation
import nicoburniske.faxion.model.{Bottom, Top}
import org.scalatest.funsuite.AnyFunSuite

class OperationTest extends AnyFunSuite {
  val pantsPath = "example/fit1/pants.jpeg"
  val poloPath  = "example/fit1/polo.jpeg"

  test("Should stitch images together properly") {
    val pants = ImmutableImage.loader().fromFile(pantsPath)
    val polo  = ImmutableImage.loader().fromFile(poloPath)

    val fit         = Seq(polo, pants)
    val fitWithTags = Seq(polo -> Top, pants -> Bottom)

    val stitched = Operation.stitchImages(fit)
    // val stitchedWithTags = Operation.stitchImagesWithTags(fitWithTags)
    // TODO: how to program assertions? ensure pant + polo are contained in generated image?
  }

  test("Crop background") {
    // val pants   = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")
    val pants   = ImmutableImage.loader().fromFile("example/fit1/pants.jpg")
    val cropped = Operation.extractForeground(pants)
    cropped.output(JpegWriter.Default, "test/1/pants.jpg")
  }

  test("fit stitching") {
    for {
      fitDir <- (1 to 3).map(ii => s"example/fit$ii/")
    } {
      val loaded   = Seq(fitDir + "shirt.jpg", fitDir + "pants.jpg").map(ImmutableImage.loader().fromFile)
      val stitched = Operation.stitchImages(loaded)
      stitched.output(JpegWriter.Default, fitDir + "stitched.jpg")
    }
  }
}
