package nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.{Grayscale, RGBColor}
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import jdk.jfr.Threshold
import nicoburniske.faxion.Article
import org.scalactic.One


import java.awt.image.BufferedImage

trait ImageClassifier[T] {
  def classify(image: ImmutableImage): Option[T]
}

object Operation extends ImageClassifier[Article] {

  def main(args: Array[String]): Unit = {
    val image   = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")

   val histogram = histogramFromImage(rgbToGrayscale(image))
    val threshold = calculateThreshold(histogram, image.width*image.height, intensitySumFromHistogram(histogram))

    applyThreshold(image, threshold).output(JpegWriter.Default, "masked.jpeg")

    //Threshold.applyThreshold()

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

  //def otsuBinarization(image: ImmutableImage): ImmutableImage = {

    //var variance: Double
    //var
   // val top = 256
    //val totalPixels = image.width*image.height

  //}

  def rgbToGrayscale(image: ImmutableImage): ImmutableImage = {
    // Equal averaging of RGB values to Grayscale
    // TODO: Perhaps change equal average to weighted average
   image.map(pixel => pixel.toColor.toGrayscale().awt())
  }

  def histogramFromImage(image: ImmutableImage): Array[Int] = {
    var intensityCounts = new Array[Int](256);
    image.forEach { pixel =>
      val pixelIntensity = (pixel.red() + pixel.blue() + pixel.green()) / 3;
      intensityCounts(pixelIntensity) = intensityCounts(pixelIntensity) + 1
    }
    intensityCounts
  }

  def intensitySumFromHistogram(histogram: Array[Int]): Int = {

    var intensitySum = 0

    for (i <- 0 to histogram.length-1) {
      intensitySum = intensitySum + histogram(i)*i
    }
    intensitySum
  }

  def calculateThreshold(histogram: Array[Int], pixelCount: Int, intensitySum: Int): Int  = {
    var meanBackground: Double = 0
    var weightBackground: Double = 0

    var meanForeground: Double = pixelCount*intensitySum
    var weightForeground = pixelCount

    var meanDifference = meanForeground-meanBackground
    var variance = weightBackground*weightForeground*Math.pow(meanDifference, 2)
    var bestVariance = Double.NegativeInfinity

    var resultantThreshold = 0

    for (intensity <- 0 to 255) {
      meanDifference = meanForeground-meanBackground
      variance = weightBackground*weightForeground*Math.pow(meanDifference, 2)

      if (variance > bestVariance) {
        bestVariance = variance
        resultantThreshold = intensity
        println(intensity)
      }

      meanBackground = (meanBackground* weightBackground + histogram(intensity) * intensity) / (weightBackground + histogram(intensity))
      meanForeground = (meanForeground* weightForeground - histogram(intensity) * intensity) / (weightForeground - histogram(intensity))
      weightBackground = weightBackground + histogram(intensity)
      weightForeground = weightForeground - histogram(intensity)

    }

    println(resultantThreshold)
    resultantThreshold

  }

  def applyThreshold(image: ImmutableImage, threshold: Int): ImmutableImage = {
    image.map(pixel => {
      val pixelIntensity = (pixel.red()+pixel.blue()+pixel.green())/3;
      if (pixelIntensity < threshold) {
        new Grayscale(255).awt()
      } else {
        new Grayscale(0).awt()
      }
    })
  }
}
