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
    None
  }

  // TODO: Decide whether mutability has significant performance improvements.
  /**
   *
   * @param images
   * @return
   */
  def stitchImages(images: Seq[Seq[ImmutableImage]]): ImmutableImage     = ???
  def stitchImages(images: Array[Array[ImmutableImage]]): ImmutableImage = ???
}
