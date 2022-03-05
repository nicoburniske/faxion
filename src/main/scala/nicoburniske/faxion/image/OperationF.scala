package nicoburniske.faxion.image

import java.awt.Color

import cats.Parallel
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.syntax.all._
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.Colors
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel

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
    val elem                              = morph.circleElem(4)
    val processed: F[Seq[ImmutableImage]] = images.map { image =>
      extractForeground(image, elem).map(_.autocrop(Color.BLACK))
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

  def extractForeground(imageF: F[ImmutableImage], shape: Set[(Int, Int)]): F[ImmutableImage] = {
    for {
      srcImage  <- imageF
      binarized  = Operation.otsuBinarization(srcImage)
      processedF = binarized
                     .pure
                     .flatMap(dilateOrErode(_, shape, false))
                     .flatMap(dilateOrErode(_, shape, true))
                     .flatMap(dilateOrErode(_, shape, true))
                     .flatMap(dilateOrErode(_, shape, false))
      processed <- processedF
      // TODO: see if pixel mutation (parallel?) is worth it.
    } yield processed.map { pixel =>
      if (pixelToIntensity(pixel) > 0) {
        srcImage.pixel(pixel.x, pixel.y).toColor.awt()
      } else {
        // transparent pixel
        Colors.Transparent.awt()
      }
    }
  }

  def pixelToIntensity(pixel: Pixel): Int = {
    (pixel.red() + pixel.blue() + pixel.green()) / 3
  }

  def dilateOrErode(imageF: F[ImmutableImage], shape: Set[(Int, Int)], dilate: Boolean): F[ImmutableImage] =
    imageF.flatMap(dilateOrErode(_, shape, dilate))

  def dilateOrErode(image: ImmutableImage, shape: Set[(Int, Int)], dilate: Boolean): F[ImmutableImage] = {
    val newImage                     = image.copy
    val awt                          = newImage.awt()
    val pixels                       = image.pixels()
    val colors: Array[Color]         = pixels.map(_.toColor.toAWT)
    val lifted: Int => Option[Color] = colors.lift

    def getColor(p: (Int, Int)): Option[Color] = {
      val (x, y) = p
      lifted(y * image.width + x)
    }

    def cond(shapeApplied: Iterable[Color]): Boolean =
      if (dilate) {
        shapeApplied.forall(_ == Color.BLACK)
      } else {
        !shapeApplied.forall(_ == Color.WHITE)
      }

    def setNewPixel(pixels: Array[Pixel]): Unit = {
      // println(s"${Thread.currentThread().getName} with ${pixels.length} to process")
      pixels.foreach { p =>
        val coordinates    = (p.x, p.y)
        val elementApplied = shape.map(addTuples(coordinates, _)).flatMap(getColor)
        val newColor       =
          if (cond(elementApplied))
            Color.BLACK.getRGB
          else
            Color.WHITE.getRGB
        awt.setRGB(p.x, p.y, newColor)
      }
    }

    // TODO: how to decide parallelism?
    val split: Seq[Array[Pixel]] = pixels.grouped(pixels.length / 10).toSeq
    val parallelSetPixel         = split.map(_.pure).map(_.map(setNewPixel)).parSequence
    parallelSetPixel.as(image)
  }

  def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val l = a._1 + b._1
    val r = a._2 + b._2
    l -> r
  }
}

object OperationF extends IOApp {
  def apply[F[_]: Async: Parallel]: OperationF[F] = new OperationF[F]

  override def run(args: List[String]): IO[ExitCode] = {
    val path     = "example/fit3/pants.jpg"
    val loadFile = Loader.imageFromFile(path)

    val before    = System.currentTimeMillis()
    val operation = OperationF[IO].extractForeground(loadFile, morph.DEFAULT_STRUCTURING_ELEMENT)
    operation
      .map(image => image.output(JpegWriter.Default, "parallel.jpg"))
      .map(_ => println(System.currentTimeMillis() - before))
      .as(ExitCode.Success)
  }
}
