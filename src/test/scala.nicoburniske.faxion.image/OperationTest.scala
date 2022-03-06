package scala.nicoburniske.faxion.image

import java.awt.Color
import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.image.Operation.collapseSets

import java.awt.image.BufferedImage
//import com.sksamuel.scrimage.color.Color
import com.sksamuel.scrimage.nio.JpegWriter
import nicoburniske.faxion.image.Operation
import nicoburniske.faxion.image.morph.{Morph, squareElem}
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

  test("morpho open shapes square 21x21") {
    val shapesImage = Operation.otsuBinarization(ImmutableImage.loader().fromFile("example/samples/shapes.jpg"))

    shapesImage.toNewBufferedImage(BufferedImage.TYPE_BYTE_BINARY)

    val erodedShapesImage = Morph.erode(shapesImage, squareElem(10));
    val openedShapesImage = Morph.dilate(erodedShapesImage, squareElem(10));

    val groundTruth = Operation.otsuBinarization(ImmutableImage.loader().fromFile("example/samples/shapes.jpg"))//"example/samples/shapes_open_square_21.jpg"))

    //groundTruth)

    val incorrectImage = openedShapesImage.map(pixel => {

      val subjectPixelBinaryIntensity = if (Operation.pixelToIntensity(pixel) >= 128) 1 else 0

      val truthPixelBinaryIntensity = if (Operation.pixelToIntensity(groundTruth.pixel(pixel.x, pixel.y)) >= 128) 1 else  0

      if (subjectPixelBinaryIntensity == truthPixelBinaryIntensity) {
        Color.BLACK
      } else {
        Color.WHITE
      }

    })

    val incorrectPixels = incorrectImage.pixels().count(_.toColor.awt() == Color.WHITE);

    incorrectImage.output(JpegWriter.Default, "example/samples/generated/big_dickens2.jpg")
   // assert(incorrectPixels == 0)



  }

  test("collapse sets - 1") {
    val setOfSets = Set(Set((6,9), (1,2)), Set((0,1), (4,5)), Set((6,3), (2,7), (1,2)))

    val collapsedSets = collapseSets(setOfSets)

    assert(collapsedSets.head == Set(6->9, (1,2),(6,3),(2,7)))

    println(collapsedSets.mkString(","))

  }
}
