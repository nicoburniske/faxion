package nicoburniske.faxion.image

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import jdk.jfr.Threshold
import nicoburniske.faxion.model.Article
import java.awt.image.{BufferedImage, FilteredImageSource, ImageFilter}

import cats.Parallel
import cats.effect.Async
import cats.syntax.all._
import javax.swing.GrayFilter

import scala.annotation.tailrec

trait ImageClassifier[F[_], T] {
  /**
   * Classifies the given image as a T
   *
   * @param image
   *   the image to classify
   * @return
   *   None if the image could not be classified. The Article type otherwise.
   */
  def classify(image: ImmutableImage): F[T]
}

class OperationF[F[_]: Async: Parallel] {

  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   *   the images of individual clothing articles
   * @return
   *   an stitched image
   */
  def stitchImages(images: Seq[F[ImmutableImage]]): F[ImmutableImage] = {
    val processed: F[Seq[ImmutableImage]] = images.map {
      _.map { image =>
        val extracted = Operation.extractForeground(image)
        extracted.autocrop(Color.black)
      }
    }.parSequence

    for {
      images    <- processed
      maxWidth   = images.map(_.width).max
      height     = images.map(_.height).sum
      blankImage = ImmutableImage.create(maxWidth, height)
    } yield overlayImages(maxWidth, blankImage, images.toList)
  }

  @scala.annotation.tailrec
  private def overlayImages(
      maxWidth: Int,
      background: ImmutableImage,
      toOverlay: List[ImmutableImage],
      height: Int = 0): ImmutableImage = {
    toOverlay match {
      case Nil                   => background
      case ::(image, restImages) =>
        val newBackground = background.overlay(image, (maxWidth - image.width) / 2, height)
        overlayImages(maxWidth, newBackground, restImages, height + image.height)
    }
  }
}

object OperationF {
  def apply[F[_]: Async: Parallel]: OperationF[F] = new OperationF[F]
}

object Operation {
  val DIMENSION                                 = 3
  val DEFAULT_MORPHOLOGY_SHAPE: Set[(Int, Int)] = {
    val range = -DIMENSION to DIMENSION
    for {
      x <- range
      y <- range
    } yield (x, y)
  }.toSet

  def main(args: Array[String]): Unit = {
    val images = Seq("example/fit1/poloSport.jpg", "example/fit1/pants.jpg")
      .map(ImmutableImage.loader.fromFile(_))
    val extracted = otsuBinarization(images(1))
    extracted.output(JpegWriter.Default, "extracted.jpg")
    val processed = erode(extracted)
    processed.output(JpegWriter.Default, "eroded.jpg")
    val full = extractForeground(images(1))
    full.output(JpegWriter.Default, "full.jpg")
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
   *   the images of individual clothing articles
   * @return
   *   an stitched image
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

    val histogram      = histogramFromImage(image)
    val intensitySum = histogram.zipWithIndex.map { case (value, index) => value * intensityToDouble(index) }.sum
    val threshold    = calcThreshold(histogram, image.width * image.height, intensitySum)
    val intThreshold = (threshold*256).toInt

    val initialResult = image.map { pixel =>
      if (pixelToIntensity(pixel) > intThreshold) // Foreground
        Color.WHITE
      else // Background.
        Color.BLACK
    }

    // Calculate a quarter-sized sub-image, centered around the original image's center
    // Set the foreground (white) to whatever group has majority values of the sub-image

    val resultSubimage = initialResult.subimage(image.width/4, image.width/4, image.width/2, image.height/2)

    val whiteCount = resultSubimage.pixels().count(_.toColor.awt() == Color.WHITE)

    if (whiteCount < (resultSubimage.width*resultSubimage.height)-whiteCount){
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
            loopVariables.cumulativeIntensitySum + (intensityToDouble(currentIntensity) * (
              histogram(currentIntensity)))

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
