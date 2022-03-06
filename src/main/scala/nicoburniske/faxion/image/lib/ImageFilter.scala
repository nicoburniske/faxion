package nicoburniske.faxion.image.lib

/**
 * image filter: transforms an image
 */
trait ImageFilter[A, B] {

  /**
   * apply the filter to an image
   */
  def filter(image: PixelImage[A]): PixelImage[B]
}
