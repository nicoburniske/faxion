package nicoburniske.faxion.image

import com.sksamuel.scrimage.ImmutableImage
import nicoburniske.faxion.Article

trait ImageClassifier[F[_],T] {
  def classify(image: ImmutableImage): F[T]
}

object Operation extends ImageClassifier[Option, Article] {

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

}
