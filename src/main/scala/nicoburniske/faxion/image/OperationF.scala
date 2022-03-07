package nicoburniske.faxion.image

import cats.Parallel
import cats.effect.kernel.syntax.all._
import cats.effect.{Async, Clock, ExitCode, IO, IOApp}
import cats.syntax.all._
import com.sksamuel.scrimage.pixels.Pixel
import nicoburniske.faxion.image.lib.{IntRGB, PixelImage, PixelImageIO, RGB}
import nicoburniske.faxion.image.morph.Morph
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

import scala.concurrent.duration._

class OperationF[F[_]: Async: Parallel: Logger: Clock] {

  /**
   * Stitch the images (of clothing articles) together to form a single "fit".
   *
   * @param images
   *   the images of individual clothing articles
   * @return
   *   an stitched image
   */
  def stitchImages(images: Seq[F[PixelImage[IntRGB]]]): F[PixelImage[IntRGB]] = {
    for {
      structuringElem <- morph.circleElem(4).pure
      processedImages <- images.map(extractForeground(_, structuringElem)).parSequence
      // .tapEach(i => i.output(JpegWriter.Default, s"${i.hashCode}-test.jpg" )) // For debugging.
      // .map(_.autocrop) // TODO: FIX

      maxWidth = processedImages.map(_.width).max
      height   = processedImages.map(_.height).sum
      empty    = PixelImage.apply(maxWidth, height, (_, _) => IntRGB.Black)
    } yield overlayImages(maxWidth, empty, processedImages.toList)
  }

  // @scala.annotation.tailrec
  def overlayImages(
      maxWidth: Int,
      background: PixelImage[IntRGB],
      toOverlay: List[PixelImage[IntRGB]],
      height: Int = 0): PixelImage[IntRGB] = {
    toOverlay match {
      case Nil                   => background
      case ::(image, restImages) =>
        //          val newBackground = background.overlay(image, (maxWidth - image.width) / 2, height)
        //          overlayImages(newBackground, restImages, height + image.height)
        // TODO add overlay
        background
    }
  }

  def extractForeground(imageF: F[PixelImage[IntRGB]], shape: Set[(Int, Int)]): F[PixelImage[IntRGB]] =
    for {
      srcImage           <- imageF
      _                  <- Logger[F].info(s"Processing image with ${srcImage.length} pixels")
      (time, binarized)  <- Async[F].delay(Operation.otsuBinarization(srcImage)).timed
      _                  <- Logger[F].info(s"Binarization took ${time.toMillis}ms")
      // processedF          = binarized.pure.flatMap(dilateOrErode(_, shape, false))
      processedF          = binarized.pure.map(Morph.erode(_, shape))
      //                              .map(Morph.dilate(_, shape))
      //                              .map(Morph.dilate(_, shape))
      //                              .map(Morph.erode(_, shape))
      //                                   .flatMap(dilateOrErode(_, shape, true))
      //                                   .flatMap(dilateOrErode(_, shape, true))
      //                                   .flatMap(dilateOrErode(_, shape, false))
      (time, processed)  <- processedF.timed
      _                  <- Logger[F].info(s"Foreground extraction took ${time.toMillis}ms")
      convert             = Async[F].delay(
                              processed.mapWithIndex {
                                case (pixel, x, y) =>
                                  if (pixel) {
                                    srcImage(x, y)
                                  } else {
                                    // TODO: what is background
                                    IntRGB.Black
                                  }
                              }
                            )
      (timeConvert, res) <- convert.timed
      _                  <- Logger[F].info(s"Finished converting in ${timeConvert.toMillis} ms")
    } yield res

  def pixelToIntensity(pixel: Pixel): Int = {
    (pixel.red() + pixel.blue() + pixel.green()) / 3
  }

  def dilateOrErode(
      imageF: F[PixelImage[Boolean]],
      shape: Set[(Int, Int)],
      dilate: Boolean): F[PixelImage[Boolean]] =
    imageF.flatMap(dilateOrErode(_, shape, dilate))

  def dilateOrErode(
      image: PixelImage[Boolean],
      shape: Set[(Int, Int)],
      dilate: Boolean): F[PixelImage[Boolean]] = {
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

    val applied = image.zipWithIndex.parMap {
      case (_, coordinates) =>
        val structuringElementApplied = shape.map(addTuples(coordinates, _)).flatMap(getColor)
        cond(structuringElementApplied)
    }

    for {
      (time, newImage) <- applied.timed
      operationName     = if (dilate) "Dilation" else "Erosion"
      _                <- Logger[F].info(s"$operationName took ${time.toMillis}ms")
    } yield newImage
  }

  def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val l = a._1 + b._1
    val r = a._2 + b._2
    l -> r
  }
}

object OperationF extends IOApp {
  private implicit def logger[F[_]: Async]: SelfAwareStructuredLogger[F] =
    Slf4jLogger.getLoggerFromClass[F](OperationF.getClass)

  def apply[F[_]: Async: Parallel]: OperationF[F] = new OperationF[F]

  override def run(args: List[String]): IO[ExitCode] = {
    val path                             = "example/fit3/pants.jpg"
    val loadFile: IO[PixelImage[RGB]]    = Loader.getPixelImageFromFile[RGB](path)
    val asIntRGB: IO[PixelImage[IntRGB]] = loadFile.map {
      _.map(p => p.toIntRGB)
    }

    val operation: IO[PixelImage[IntRGB]] =
      OperationF[IO].extractForeground(asIntRGB, morph.DEFAULT_STRUCTURING_ELEMENT)

    for {
      result <- operation
      asRGB   = result.map(p => p.toRGB)
      _      <- IO.fromTry(PixelImageIO.write(asRGB, "parallel2.png")).timeout(10.seconds)
    } yield ExitCode.Success
  }
}
