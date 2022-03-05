package nicoburniske.faxion.image

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import nicoburniske.faxion.image.morph.Morph
import nicoburniske.faxion.model.Article

import scala.annotation.tailrec

trait ImageClassifier[F[_], T] {
  /**
   * Classifies the given image as a T
   *
   * @param image
   * the image to classify
   * @return
   * None if the image could not be classified. The Article type otherwise.
   */
  def classify(image: ImmutableImage): F[T]
}


object Operation {

  def main(args: Array[String]): Unit = {
    val images = Seq("example/fit1/shirt.jpg", "example/fit3/pants.jpg")
      .map(ImmutableImage.loader.fromFile(_))
    val extracted = otsuBinarization(images(1)).scale(0.5)
    extracted.output(JpegWriter.Default, "extracted.jpg")
    val processed = Morph.dilate(Morph.erode(extracted)).scale(2)
    processed.output(JpegWriter.Default, "dilated.jpg")
    val full = extractForeground(images(1))
    full.output(JpegWriter.Default, "done.jpg")
    //
    //    val combined = stitchImages(images)
    //
    //    combined.output(JpegWriter.Default, "combined.jpg")
  }


  // TODO: Decide whether mutability has significant performance improvements.
  // TODO: Py implement
  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   * the images of individual clothing articles
   * @return
   * an stitched image
   */
  def stitchImagesWithTags(images: Seq[(ImmutableImage, Article)]): ImmutableImage = ???

  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   * the images of individual clothing articles
   * @return
   * an stitched image
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
    val transparent = new Color(1f, 1f, 1f, 1)
    val binarization = otsuBinarization(image).scale(1 / scaleFactor)
    Morph.dilate(binarization).scale(scaleFactor).map(pixel =>
      if (pixelToIntensity(pixel) > 0) {
        image.pixel(pixel.x, pixel.y).toColor.awt()
      } else {
        // transparent pixel
        transparent
      })
  }

  def otsuBinarization(image: ImmutableImage): ImmutableImage = {
    val greyscaleImage = image.map(pixel => pixel.toColor.toGrayscale.awt())
    val histogram = histogramFromImage(greyscaleImage)
    val intensitySum = histogram.zipWithIndex.map { case (value, index) => value * index }.sum
    val threshold = calculateThreshold(histogram, image.width * image.height, intensitySum)

    // Apply threshold.
    image.map { pixel =>
      // Foreground.
      if (pixelToIntensity(pixel) <= threshold)
        Color.WHITE
      // Background.
      else
      Color.BLACK
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

  def calculateThreshold(histogram: Array[Int], pixelCount: Int, intensitySum: Int): Int = {
    var meanBackground: Double = 0
    var weightBackground: Double = 0

    var meanForeground: Double = pixelCount * intensitySum
    var weightForeground = pixelCount

    var bestVariance = Double.NegativeInfinity

    var resultantThreshold = 0

    for (intensity <- 0 to 255) {
      val meanDifference = meanForeground - meanBackground
      val currentVariance = weightBackground * weightForeground * Math.pow(meanDifference, 2)

      meanBackground =
        (meanBackground * weightBackground + histogram(intensity) * intensity) / (weightBackground + histogram(intensity))
      meanForeground =
        (meanForeground * weightForeground - histogram(intensity) * intensity) / (weightForeground + histogram(intensity))
      weightBackground = weightBackground + histogram(intensity)
      weightForeground = weightForeground - histogram(intensity)

      if (currentVariance > bestVariance) {
        bestVariance = currentVariance
        resultantThreshold = intensity
      }
    }

    resultantThreshold
  }

  case class LoopVariables(
                            weightBackground: Int,
                            weightForeground: Int,
                            cumulativeIntensitySum: Int,
                            maximumVariance: Double,
                            threshold: Int) {}

  object LoopVariables {
    def apply(): LoopVariables = LoopVariables(0, 0, 0, Double.NegativeInfinity, 0)
  }

  def calcThreshold(histogram: Array[Int], pixelCount: Int, intensitySum: Int): Int = {

    @tailrec
    def continue(loopVariables: LoopVariables, intensities: List[Int]): Int = {
      intensities match {
        case ::(currentIntensity, nextIntensities) =>
          val newWeightBackground = loopVariables.weightBackground + histogram(currentIntensity)
          val newWeightForeground = loopVariables.weightForeground - newWeightBackground

          if (newWeightBackground == 0) {
            continue(loopVariables.copy(weightBackground = newWeightBackground), nextIntensities)
          } else if (newWeightForeground == 0) {
            loopVariables.threshold
          } else {

            val newCumulativeIntensitySum =
              loopVariables.cumulativeIntensitySum + (currentIntensity * histogram(currentIntensity))

            // val meanBackground = newCumulativeIntensitySum / newWeightBackground
            // val meanForeground = (intensitySum - newCumulativeIntensitySum) / newWeightForeground
            val meanBackground =
            (newWeightBackground + histogram(currentIntensity) * currentIntensity) / (newWeightBackground + histogram(
              currentIntensity))
            val meanForeground =
              (newWeightForeground - histogram(currentIntensity) * currentIntensity) / (newWeightForeground + histogram(
                currentIntensity))

            val meanDifference = meanBackground - meanForeground

            val variance = newWeightBackground * newWeightForeground * Math.pow(meanDifference, 2)

            if (variance > loopVariables.maximumVariance) {

              val nextLoopVariables = LoopVariables(
                newWeightBackground,
                newWeightForeground,
                newCumulativeIntensitySum,
                variance,
                currentIntensity)
              continue(nextLoopVariables, nextIntensities)

            } else {

              val nextLoopVariables = LoopVariables(
                newWeightBackground,
                newWeightForeground,
                newCumulativeIntensitySum,
                loopVariables.maximumVariance,
                loopVariables.threshold)
              continue(nextLoopVariables, nextIntensities)

            }
          }
        case Nil => loopVariables.threshold
      }

    }

    continue(LoopVariables(0, weightForeground = pixelCount, 0, Double.NegativeInfinity, 0), (0 until 256).toList)
  }


  def pixelToIntensity(pixel: Pixel): Int = {
    (pixel.red() + pixel.blue() + pixel.green()) / 3
  }
}
