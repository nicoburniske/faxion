package nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.image.lib.{IntRGB, PixelImage, RGB}
import nicoburniske.faxion.image.morph.Morph
import nicoburniske.faxion.model.Article

import scala.annotation.tailrec

object Operation {
  def main(args: Array[String]): Unit = {
    //    val images =
    //      Seq("example/fit3/pants.jpg", "example/fit3/shirt.jpg").map(PixelImageIO.getPixelImageFromFile[RGB](_))
    //    val before = System.currentTimeMillis()
    //    val full = extractForeground(images(0), 1.0)
    //    println(System.currentTimeMillis() - before)
    //    PixelImageIO.write(full, "full.jpg")
  }

  // 89842

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
  def stitchImages(images: Seq[PixelImage[IntRGB]]): PixelImage[IntRGB] = {
    // @formatter:off
    val maxWidth   = images.map(_.width).max
    val height     = images.map(_.height).sum

    // @scala.annotation.tailrec
    def overlayImages(background: PixelImage[IntRGB], toOverlay: List[PixelImage[IntRGB]], height: Int = 0): PixelImage[IntRGB] = {
      toOverlay match {
        case Nil => background
        case ::(image, restImages) =>
//          val newBackground = background.overlay(image, (maxWidth - image.width) / 2, height)
//          overlayImages(newBackground, restImages, height + image.height)
          // TODO add overlay
          background
      }
    }

    val processedImages = images
      .map(extractForeground(_))
      // .tapEach(i => i.output(JpegWriter.Default, s"${i.hashCode}-test.jpg" )) // For debugging.
      // .map(_.autocrop) // TODO: FIX

    val empty = PixelImage.apply(maxWidth, height, (_, _) => IntRGB.Black)
    overlayImages(empty, processedImages.toList)
    // @formatter:on
  }

  // TODO: Scalefactor
  def extractForeground(image: PixelImage[IntRGB], scaleFactor: Double = 2.0): PixelImage[IntRGB] = {
    assert(scaleFactor > 0)
    val binarization = otsuBinarization(image)
    val threadName   = Thread.currentThread.getName
    println(s"$threadName-1")
    val processed    = Morph.erode(binarization)
    println(s"$threadName-2")
    val processed2   = Morph.dilate(processed)
    println(s"$threadName-3")
    val processed3   = Morph.dilate(processed2)
    println(s"$threadName-4")
    val processed4   = Morph.erode(processed3)
    processed4.zipWithIndex.map {
      case (isForeground, (x, y)) =>
        if (isForeground) {
          image.valueAt(x, y)
        } else {
          // transparent pixel
          // TODO : make transparent
          IntRGB.Black
        }
    }
  }

  def otsuBinarization(image: PixelImage[IntRGB]): PixelImage[Boolean] = {
    val histogram    = histogramFromImage(image)
    val intensitySum =
      histogram.zipWithIndex.map { case (value, index) => value * intensityToDouble(index) }.sum
    val threshold    = calcThreshold(histogram, image.width * image.height, intensitySum)
    val intThreshold = (threshold * 256).toInt

    val initialResult = image.map { pixel => pixel.gray > intThreshold }

    // Calculate a quarter-sized sub-image, centered around the original image's center
    // Set the foreground (white) to whatever group has majority values of the sub-image

    //    val resultSubimage =
    //      initialResult.subimage(image.width / 4, image.width / 4, image.width / 2, image.height / 2)
    //
    //    val whiteCount = resultSubimage.pixels().count(_.toColor.awt() == Color.WHITE)
    //
    //    if (whiteCount < (resultSubimage.width * resultSubimage.height) - whiteCount) {
    //      initialResult.map(pixel => {
    //        if (pixel.toColor.awt() == Color.WHITE) {
    //          Color.BLACK
    //        } else {
    //          Color.WHITE
    //        }
    //
    //      })
    //
    //    } else {
    //      initialResult
    //    }

    initialResult
  }

  def histogramFromImage(image: PixelImage[IntRGB]): Array[Int] = {
    val intensityCounts = new Array[Int](256)
    image.map { pixel =>
      intensityCounts(pixel.gray) = intensityCounts(pixel.gray) + 1
      ()
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

  // TODO FIX.
  def pixelToIntensity(pixel: IntRGB): Int = {
    (pixel.r + pixel.g + pixel.b) / 3
  }

  def intensityToDouble(intensity: Int): Double = {
    intensity / 255.0
  }
}
