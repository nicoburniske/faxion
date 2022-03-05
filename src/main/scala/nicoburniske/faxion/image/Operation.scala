package nicoburniske.faxion.image

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import nicoburniske.faxion.image.morph.Morph
import nicoburniske.faxion.model.Article

import scala.annotation.tailrec

object Operation {
  def main(args: Array[String]): Unit = {
    val images =
      Seq("example/fit3/pants.jpg", "example/fit3/shirt.jpg").map(ImmutableImage.loader.fromFile(_))
    val full   = extractForeground(images(0), 4.0)
    full.output(JpegWriter.Default, "full.jpg")
  }

  // TODO: Decide whether mutability has significant performance improvements.
  // TODO: Py implement
  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   *   the images of individual clothing articles
   * @return
   *   an stitched image
   */
  def stitchImagesWithTags(images: Seq[(ImmutableImage, Article)]): ImmutableImage = ???

  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   *   the images of individual clothing articles
   * @return
   *   an stitched image
   */
  def stitchImages(images: Seq[ImmutableImage]): ImmutableImage = {
    // @formatter:off
    val maxWidth   = images.map(_.width).max
    val height     = images.map(_.height).sum

    @scala.annotation.tailrec
    def overlayImages(background: ImmutableImage, toOverlay: List[ImmutableImage], height: Int = 0): ImmutableImage = {
      toOverlay match {
        case Nil => background
        case ::(image, restImages) =>
          val newBackground = background.overlay(image, (maxWidth - image.width) / 2, height)
          overlayImages(newBackground, restImages, height + image.height)
      }
    }

    val processedImages = images
      .map(extractForeground(_))
      // .tapEach(i => i.output(JpegWriter.Default, s"${i.hashCode}-test.jpg" )) // For debugging.
      .map(_.autocrop)

    val blankImage = ImmutableImage.create(maxWidth, height)
    overlayImages(blankImage, processedImages.toList)
    // @formatter:on
  }

  def extractForeground(image: ImmutableImage, scaleFactor: Double = 2.0): ImmutableImage = {
    assert(scaleFactor > 0)
    val transparent  = new Color(1f, 1f, 1f, 1)
    val binarization = otsuBinarization(image).scale(1 / scaleFactor)
    println("1")
    val processed    = Morph.erode(binarization)
    println("2")
    val processed2   = Morph.dilate(processed)
    println("3")
    val processed3   = Morph.dilate(processed2)
    println("4")
    val processed4   = Morph.erode(processed3)
    processed4
      .scale(scaleFactor)
      .map(pixel =>
        if (pixelToIntensity(pixel) > 0) {
          image.pixel(pixel.x, pixel.y).toColor.awt()
        } else {
          // transparent pixel
          transparent
        })
  }

  def otsuBinarization(image: ImmutableImage): ImmutableImage = {

    val histogram    = histogramFromImage(image)
    val intensitySum =
      histogram.zipWithIndex.map { case (value, index) => value * intensityToDouble(index) }.sum
    val threshold    = calcThreshold(histogram, image.width * image.height, intensitySum)
    val intThreshold = (threshold * 256).toInt

    val initialResult = image.map { pixel =>
      if (pixelToIntensity(pixel) > intThreshold) // Foreground
        Color.WHITE
      else                                        // Background.
        Color.BLACK
    }

    // Calculate a quarter-sized sub-image, centered around the original image's center
    // Set the foreground (white) to whatever group has majority values of the sub-image

    val resultSubimage =
      initialResult.subimage(image.width / 4, image.width / 4, image.width / 2, image.height / 2)

    val whiteCount = resultSubimage.pixels().count(_.toColor.awt() == Color.WHITE)

    if (whiteCount < (resultSubimage.width * resultSubimage.height) - whiteCount) {
      initialResult.map(pixel => {
        if (pixel.toColor.awt() == Color.WHITE) {
          Color.BLACK
        } else {
          Color.WHITE
        }

      })

    } else {
      initialResult
    }

  }

  def histogramFromImage(image: ImmutableImage): Array[Int] = {
    val intensityCounts = new Array[Int](256)
    image.forEach { pixel =>
      val pixelIntensity = pixelToIntensity(pixel)
      intensityCounts(pixelIntensity) = intensityCounts(pixelIntensity) + 1
    }
    intensityCounts
  }

  case class LoopVariables(
      weightBackground: Double,
      weightForeground: Double,
      cumulativeIntensitySum: Double,
      maximumVariance: Double,
      threshold: Double) {}

  object LoopVariables {
    def apply(): LoopVariables = LoopVariables(0, 0, 0, Double.NegativeInfinity, 0.0)
  }

  def calcThreshold(histogram: Array[Int], pixelCount: Int, intensitySum: Double): Double = {

    @tailrec
    def continue(loopVariables: LoopVariables, intensities: List[Int]): Double = {
      intensities match {
        case ::(currentIntensity, nextIntensities) =>
          val newWeightForeground = pixelCount - loopVariables.weightBackground

          var newMaximumVariance = loopVariables.maximumVariance
          var newThreshold       = loopVariables.threshold
          if (loopVariables.weightBackground > 0 && newWeightForeground > 0) {

            val meanForeground = (intensitySum - loopVariables.cumulativeIntensitySum) / newWeightForeground

            val variance =
              loopVariables.weightBackground * newWeightForeground * ((loopVariables.cumulativeIntensitySum / loopVariables.weightBackground) - meanForeground) * ((loopVariables.cumulativeIntensitySum / loopVariables.weightBackground) - meanForeground)
            if (variance >= loopVariables.maximumVariance) {
              newThreshold = intensityToDouble(currentIntensity)
              newMaximumVariance = variance
            }

          }
          val newWeightBackground       =
            loopVariables.weightBackground + histogram(currentIntensity)
          val newCumulativeIntensitySum =
            loopVariables.cumulativeIntensitySum + (intensityToDouble(currentIntensity) * (histogram(
              currentIntensity)))

          val nextLoopVariables = LoopVariables(
            newWeightBackground,
            newWeightForeground,
            newCumulativeIntensitySum,
            newMaximumVariance,
            newThreshold)
          continue(nextLoopVariables, nextIntensities)

        case Nil => loopVariables.threshold
      }

    }

    continue(
      LoopVariables(0, weightForeground = pixelCount, 0, Double.NegativeInfinity, 0),
      (0 until 256).toList)
  }

  def pixelToIntensity(pixel: Pixel): Int = {
    (pixel.red() + pixel.blue() + pixel.green()) / 3
  }

  def intensityToDouble(intensity: Int): Double = {
    intensity / 255.0
  }

}
