package nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.Article

trait ImageClassifier[T] {
  def classify(image: ImmutableImage): Option[T]
}

object Operation extends ImageClassifier[Article] {

  /**
   * Classifies the given image as subtype of Article [
   * @param image
   *   the image to classify
   * @return
   *   None if the image could not be classified. The Article type otherwise.
   */
  override def classify(image: ImmutableImage): Option[Article] = {
    // TODO: Py implement
    None
  }

  // TODO: Decide whether mutability has significant performance improvements.
  // TODO: Py implement
  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * Rows of images (Seq[ImmutableImage]) should be stitched together horizontally.
   *
   * @param images
   *   the images of individual clothing articles
   * @return
   *   an stitched image
   */
  def stitchImages(images: Seq[Seq[ImmutableImage]]): ImmutableImage     = ???
  def stitchImages(images: Array[Array[ImmutableImage]]): ImmutableImage = ???
}
