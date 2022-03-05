package nicoburniske.faxion.image

import cats.Parallel
import cats.effect.Async
import cats.syntax.all._
import com.sksamuel.scrimage.ImmutableImage

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
        extracted.autocrop()
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
