package nicoburniske.faxion.image

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import nicoburniske.faxion.Article

trait ImageClassifier[T] {
  def classify(image: ImmutableImage): Option[T]
}

object Operation extends ImageClassifier[Article] {

  def main(args: Array[String]): Unit = {
    val image = ImmutableImage.loader().fromFile("example/fit2/pants.jpg")
    otsuBinarization(image).output(JpegWriter.Default, "masked.jpeg")
    // Threshold.applyThreshold()
  }

  /**
   * Classifies the given image as subtype of Article [
   * @param image
   *   the image to classify
   * @return
   *   None if the image could not be classified. The Article type otherwise.
   */
  override def classify(image: ImmutableImage): Option[Article] = {
    None
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
  def stitchImages(images: Seq[ImmutableImage]): ImmutableImage                    = ???
  def stitchImages(images: Array[ImmutableImage]): ImmutableImage                  = ???

  def extractForeground(image: ImmutableImage): ImmutableImage = {
    val otsu = otsuBinarization(image)
    // TODO: finish
    null
  }

  def otsuBinarization(image: ImmutableImage): ImmutableImage = {
    // TODO: Perhaps change equal average to weighted average
    val greyscaleImage = image.map(pixel => pixel.toColor.toGrayscale.awt())
    val histogram      = histogramFromImage(greyscaleImage)
    val intensitySum   = histogram.zipWithIndex.map { case (value, index) => value * index }.sum
    val threshold      = calculateThreshold(histogram, image.width * image.height, intensitySum)

    // Apply threshold.
    image.map { pixel =>
      if (pixelToIntensity(pixel) < threshold)
        Color.BLACK
      else
        Color.WHITE
    }
  }

  def histogramFromImage(image: ImmutableImage): Array[Int] = {
    val intensityCounts = new Array[Int](256)
    // image.pixels().map(pixelToIntensity).groupBy(identity).view.mapValues(_.length).toMap
    image.forEach { pixel =>
      val pixelIntensity = pixelToIntensity(pixel)
      intensityCounts(pixelIntensity) = intensityCounts(pixelIntensity) + 1
    }
    intensityCounts
  }

  def calculateThreshold(histogram: Array[Int], pixelCount: Int, intensitySum: Int): Int = {
    var meanBackground: Double   = 0
    var weightBackground: Double = 0

    var meanForeground: Double = pixelCount * intensitySum
    var weightForeground       = pixelCount

    var bestVariance = Double.NegativeInfinity

    var resultantThreshold = 0

    for (intensity <- 0 to 255) {
      val meanDifference  = meanForeground - meanBackground
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
        println(intensity)
      }
    }

    println(resultantThreshold)
    resultantThreshold
  }

  def pixelToIntensity(pixel: Pixel): Int = {
    (pixel.red() + pixel.blue() + pixel.green()) / 3
  }
}
