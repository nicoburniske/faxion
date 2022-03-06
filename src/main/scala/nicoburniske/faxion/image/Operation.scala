package nicoburniske.faxion.image

import java.awt.Color
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.filter.BlurFilter
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel
import nicoburniske.faxion.image.morph.{Morph, circleElem, crossElement, squareElem}
import nicoburniske.faxion.model.Article

import scala.annotation.tailrec
import scala.collection.mutable

object Operation {
  def main(args: Array[String]): Unit = {
    val images =
      Seq("example/fit2/pants.jpg", "example/fit3/shirt.jpg").map(ImmutableImage.loader.fromFile(_))
    val full = extractForeground(images(0), 8.0)

    full.output(JpegWriter.Default, "full.jpg")
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
    val maxWidth = images.map(_.width).max
    val height = images.map(_.height).sum

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
    val blurredImage = image.filter(new BlurFilter())
    val binarization = otsuBinarization(blurredImage).scale(1 / scaleFactor)

    binarization.output(JpegWriter.Default, "bi.jpg")
    val labels = labelConnectedComponents(binarization)
    println(s"Largest component ${labels.head.size} = ${labels.headOption.map(_.toString).getOrElse("")}")
    println(labels.mkString(","))

    val threadName = Thread.currentThread.getName
    println(s"$threadName-1")
    val processed = Morph.erode(binarization, squareElem(1))
    println(s"$threadName-2")
    val processed2 = Morph.dilate(processed, squareElem(2))
    println(s"$threadName-3")
    val processed3 = Morph.dilate(processed2, circleElem(4))
    println(s"$threadName-4")
    val processed4 = Morph.erode(processed3, circleElem(4))
    println(s"$threadName-1")
    val processed5 = Morph.erode(processed4, circleElem(4))
    println(s"$threadName-2")
    val processed6 = Morph.dilate(processed5, circleElem(4))
    processed6
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

    val histogram = histogramFromImage(image)
    val intensitySum =
      histogram.zipWithIndex.map { case (value, index) => value * intensityToDouble(index) }.sum
    val threshold = calcThreshold(histogram, image.width * image.height, intensitySum)
    val intThreshold = (threshold * 256).toInt

    val initialResult = image.map { pixel =>
      if (pixelToIntensity(pixel) > intThreshold) // Foreground
        Color.WHITE
      else // Background.
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
          var newThreshold = loopVariables.threshold
          if (loopVariables.weightBackground > 0 && newWeightForeground > 0) {

            val meanForeground = (intensitySum - loopVariables.cumulativeIntensitySum) / newWeightForeground

            val variance =
              loopVariables.weightBackground * newWeightForeground * ((loopVariables.cumulativeIntensitySum / loopVariables.weightBackground) - meanForeground) * ((loopVariables.cumulativeIntensitySum / loopVariables.weightBackground) - meanForeground)
            if (variance >= loopVariables.maximumVariance) {
              newThreshold = intensityToDouble(currentIntensity)
              newMaximumVariance = variance
            }

          }
          val newWeightBackground =
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

  def labelConnectedComponents(binaryImage: ImmutableImage, maxLabels: Int = 16): Seq[Set[(Int, Int)]] = {

    val whitePixels: List[Pixel] = binaryImage.pixels().filter(_.toColor.awt() == Color.WHITE).toList

    @tailrec
    def continue(connectedGroups: Set[Set[(Int, Int)]], pixels: List[Pixel]): Set[Set[(Int, Int)]] = {
      pixels match {
        case ::(currentPixel, nextPixels) =>
          // println(nextPixels.size +" remaining")

          val currentPixelX = currentPixel.x
          val currentPixelY = currentPixel.y

          val sq = squareElem(1).filter { case (x, y) => x < 0 && y < 0 };
          // println(sq.size)
          // Get a list of neighboring pixels
          // Filter neighboring pixels to get only white pixels
          val neighboringWhitePixelSets = {

            sq.map { case (x, y) => (currentPixelX + x, currentPixelY + y) }
              .filter {
                case (x, y) =>
                  (x > 0 && x < binaryImage.width) && (y > 0 && y < binaryImage.height) && binaryImage
                    .pixel(x, y)
                    .toColor
                    .awt() == Color.WHITE
              }
              .flatMap(pixelLocation => {
                connectedGroups.filter(_.contains(pixelLocation))
              })

          }

          val otherSets = connectedGroups diff neighboringWhitePixelSets

          val reducedNeighborSet: Set[(Int, Int)] =
            if (neighboringWhitePixelSets.nonEmpty)
              neighboringWhitePixelSets.toList.reduce((a, b) => a union b)
            else Set.empty

          val combinedNeighborSet = reducedNeighborSet union Set((currentPixelX, currentPixelY))

          continue(otherSets union Set(combinedNeighborSet), nextPixels)

        /*
                    if (neighboringWhitePixelSets.size > 8) {
                      //println(neighboringWhitePixelSets.size);
                    }

                    if (neighboringWhitePixelSets.nonEmpty) {

                      val nonNeighboringConnectedSets = connectedGroups

                    } else {

                    }

                    if (neighboringWhitePixelSets.size == 1) {

                      val otherSets = connectedGroups diff neighboringWhitePixelSets
                      val newSet    = neighboringWhitePixelSets.head union Set((currentPixelX, currentPixelY))

                      //val newwy = Set((currentPixelX, currentPixelY), ...neighboringWhitePixelSets.head)

                      //println("o: " +otherSets.mkString(","))
                      //println("n: " +newSet.mkString(","))

                      continue(otherSets union Set(newSet), nextPixels)
                      // add pixel to that set
                    } else if (neighboringWhitePixelSets.size > 1) {

                      val combinedEquivalents = neighboringWhitePixelSets.toList.reduce((a, b) => a union b);

                      val newSet = combinedEquivalents union Set((currentPixelX, currentPixelY))
                      //println(newSet.mkString(","))
                      // union the sets
                      // add the pixel to the resulting set
                      continue(connectedGroups union Set(newSet), nextPixels);

                    } else {

                      // create a new set and add the pixel to that set
                      continue(connectedGroups union Set(Set((currentPixelX, currentPixelY))), nextPixels);

                    }

                    // neighboringWhitePixels.filter(connectedGroups contains())

                    // Check if the white neighboring pixels are in a set in connectedGroups
                    // compare the different sets
                    // if the sets are different, union them

                    // if only one set exists, add the current pixel to that set

                    // continue(connectedGroups, nextPixels)
         */
        case Nil => connectedGroups
      }
    }

    val firstPassResults = continue(Set.empty, whitePixels)

    // FLATMAP
    // F[A].flatMap(A -> F[B]): F[B]

    // EXAMPLE
    // List("a", "ab").flatMap(ii => List(ii.length, ii.length + 1)) = List(1, 2, 2, 3)
    // List[String].flatMap(String -> List[Int]): List[Int]


    // firstPassResults.toList.fo((a,b) => {
    //  if (a.count(aPosition => b.contains(aPosition)) > 0){
    //     a union b
    // } else {

    // }

    // })




    println(s"Found ${firstPassResults.size} sets in first pass.")
    val groupedBySize = firstPassResults.groupBy(_.size).toSeq.sortWith(_._1> _._1)
    // val largest = groupedBySize.head._2.toSeq.sortWith(_.size > _.size)
    groupedBySize.foreach { case (size, sets) => println(s"found ${sets.size} with size $size") }
    collapseSets(firstPassResults)
  }

  // TODO: unit tests.
  def collapseSets(sets: Set[Set[(Int, Int)]]): Seq[Set[(Int, Int)]] = {
    val seen: mutable.Set[Set[(Int, Int)]] = mutable.Set()
    val worklist: mutable.Stack[Set[(Int, Int)]] = mutable.Stack()
    val result: mutable.Set[Set[(Int, Int)]] = mutable.Set()

    // Initialize worklist.
    sets.foreach(worklist.push)

    while (worklist.nonEmpty) {
      val curr: Set[(Int, Int)] = worklist.pop()
      val anyNew = worklist.iterator.exists { other =>
        if (other.isEmpty) {
          println("Empty Set")
        }
        val intersection = curr intersect other
        lazy val newCombo = curr union other
        lazy val isNew = !seen.contains(newCombo)
        if (intersection.nonEmpty && isNew) {
          seen.add(newCombo)
          worklist.push(newCombo)
          true
        } else {
          false
        }
      }
      if (!anyNew)
        result.add(curr)
    }
    // result.remove()
    sets.foreach(result.remove)
    result.toSeq.sortWith(_.size > _.size)
  }
  /*
  HashSet((56,81), (96,121), (76,101), (82,107), (100,125), (77,102), (47,72), (106,131), (33,58), (67,92), (85,110), (45,70), (61,86), (102,127), (101,126), (48,73), (104,129), (94,119), (37,62), (34,59), (92,117), (99,124), (84,109), (95,120), (90,115), (97,122), (40,65), (54,79), (65,90), (42,67), (89,114), (50,75), (71,96), (62,87), (108,133), (72,97), (52,77), (91,116), (59,84), (57,82), (58,83), (66,91), (75,100), (38,63), (36,61), (74,99), (46,71), (87,112), (70,95), (83,108), (98,123), (49,74), (44,69), (68,93), (51,76), (32,57), (60,85), (63,88), (88,113), (64,89), (105,130), (55,80), (93,118), (86,111), (31,56), (69,94), (79,104), (43,68), (78,103), (41,66), (80,105), (53,78), (81,106), (73,98), (107,132), (103,128), (35,60), (39,64)),
   */
}