package nicoburniske.faxion.image.morph

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage

object Morph {

  /**
   * Ensure image is grayscale. Dilates image by the structuring element.
   *
   * @param image
   *   the image to dilate morphologically
   * @param shape
   *   the structuring element
   * @return
   *   the dilated image
   */
  def dilate(image: ImmutableImage, shape: Set[(Int, Int)] = DEFAULT_STRUCTURING_ELEMENT): ImmutableImage = {
    dilateOrErode(image, shape)
  }

  /**
   * Ensure image is grayscale. Erodes image by the structuring element.
   *
   * @param image
   *   the image to erode morphologically
   * @param shape
   *   the structuring element
   * @return
   *   the dilated image
   */
  def erode(image: ImmutableImage, shape: Set[(Int, Int)] = DEFAULT_STRUCTURING_ELEMENT): ImmutableImage = {
    dilateOrErode(image, shape, dilate = false)
  }

  private def dilateOrErode(
      image: ImmutableImage,
      shape: Set[(Int, Int)] = DEFAULT_STRUCTURING_ELEMENT,
      dilate: Boolean = true): ImmutableImage = {
    val colors                       = image.pixels().map(_.toColor.toAWT)
    val lifted: Int => Option[Color] = colors.lift

    def getColor(p: (Int, Int)): Option[Color] = {
      val (x, y) = p
      lifted(y * image.width + x)
    }

    def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
      val l = a._1 + b._1
      val r = a._2 + b._2
      l -> r
    }

    def cond(shapeApplied: Iterable[Color]): Boolean =
      if (dilate) {
        shapeApplied.forall(_ == Color.BLACK)
      } else {
        !shapeApplied.forall(_ == Color.WHITE)
      }

    image.map { pixel =>
      val coordinates               = (pixel.x, pixel.y)
      val structuringElementApplied = shape.map(addTuples(coordinates, _)).flatMap(getColor)
      if (cond(structuringElementApplied))
        Color.BLACK
      else
        Color.WHITE
    }
  }
}
