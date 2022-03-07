package nicoburniske.faxion.image.morph

import nicoburniske.faxion.image.lib.PixelImage

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
  def dilate(
      image: PixelImage[Boolean],
      shape: Set[(Int, Int)] = DEFAULT_STRUCTURING_ELEMENT): PixelImage[Boolean] = {
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
  def erode(
      image: PixelImage[Boolean],
      shape: Set[(Int, Int)] = DEFAULT_STRUCTURING_ELEMENT): PixelImage[Boolean] = {
    dilateOrErode(image, shape, dilate = false)
  }

  private def dilateOrErode(
      image: PixelImage[Boolean],
      shape: Set[(Int, Int)],
      dilate: Boolean = true): PixelImage[Boolean] = {

    def getColor(p: (Int, Int)): Option[Boolean] = {
      // TODO: make this better. Is there an option getter?
      val (x, y) = p
      if (image.contains(x, y)) {
        Some(image(x, y))
      } else {
        None
      }
    }

    def cond(shapeApplied: Iterable[Boolean]): Boolean =
      if (dilate) {
        shapeApplied.forall(!_)
      } else {
        !shapeApplied.forall(identity)
      }

    image.zipWithIndex.map {
      case (_, coordinates) =>
        val structuringElementApplied = shape.map(addTuples(coordinates, _)).flatMap(getColor)
        cond(structuringElementApplied)
    }
  }

  def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val l = a._1 + b._1
    val r = a._2 + b._2
    l -> r
  }
}
