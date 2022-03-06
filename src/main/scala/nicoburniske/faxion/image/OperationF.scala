package nicoburniske.faxion.image

import java.awt.Color
import java.awt.image.BufferedImage

import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.syntax.all._
import cats.{Defer, Parallel}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.Colors
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.pixels.Pixel

class OperationF[F[_]: Async: Parallel: Defer] {

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
                     .map(_.scale(0.5))
                     .flatMap(dilateOrErode(_, shape, false))
                     .flatMap(dilateOrErode(_, shape, true))
                     .flatMap(dilateOrErode(_, shape, true))
                     .flatMap(dilateOrErode(_, shape, false))
                     .map(_.scale(2.0))
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
    val before                       = System.currentTimeMillis()
    val newImage                     = image.copy
    val awt                          = newImage.awt()
    val pixels                       = image.pixels()
    println(s"pixels ${pixels.length}")
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

    def handlePixel(p: Pixel): Unit = {
      // println(Thread.currentThread().getName)
      val coordinates    = (p.x, p.y)
      val elementApplied = shape.map(addTuples(coordinates, _)).flatMap(getColor)
      val newColor       =
        if (cond(elementApplied))
          Color.BLACK.getRGB
        else
          Color.WHITE.getRGB
      awt.setRGB(p.x, p.y, newColor)
    }

    val parallelSet = pixels.toSeq.map(p => Defer[F].defer(handlePixel(p).pure)).parSequence
    parallelSet.map(_ => println(System.currentTimeMillis() - before)).as(newImage)
  }

  def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val l = a._1 + b._1
    val r = a._2 + b._2
    l -> r
  }

  def withPixelImage(bf: BufferedImage): ImmutableImage = {
////    val immut      = ImmutableImage.fromAwt(bf)
////    val otsu       = Operation.otsuBinarization(immut)
////    val pixelImage = ConverterDouble.toPixelImage(otsu.awt())
//
//    val size                  = 2
////    val asSet                 = morph.squareElem(size).map(addTuples(_, (size + 1, size + 1)))
////    println(asSet.mkString(", "))
//    // val structuringElement: (Int, Int) => Boolean = asSet.contains(_, _)
//    // val asFunc                                    = PixelImage(4, 4, structuringElement)
//    val emptyImage            = PixelImage.apply(3, 3, (_, _) => 1.0)
//    val erosionBox            = Erosion.box(2)
//    val boxStructuringElement = MorphologicalFilter.boxElement(3)
//    println(boxStructuringElement.toString)
//    val boxDouble             = boxStructuringElement.map { p =>
//      if (p) // 0 is black
//        1.0
//      else
//        0.0
//    }
//    // val eroded                = Erosion.apply(boxStructuringElement).filter(pixelImage)
//    // val asDouble                                  = asFunc.map(if (_) 1.0 else 0.0)
//    val res                   = ConverterDouble.toBufferedImage(emptyImage)
//    ImmutableImage.fromAwt(res)
    ???
  }
}

object OperationF extends IOApp {
  def apply[F[_]: Async: Parallel]: OperationF[F] = new OperationF[F]

  override def run(args: List[String]): IO[ExitCode] = {
    val path     = "example/fit2/pants.jpg"
    val loadFile = Loader.imageFromFile(path)

    val operation = OperationF[IO].extractForeground(loadFile, morph.DEFAULT_STRUCTURING_ELEMENT)
    operation
      .map(image => image.output(JpegWriter.Default, "parallel.jpg"))
      // .map(_ => println(System.currentTimeMillis() - before))
      .as(ExitCode.Success)
//    val bf     = Loader.bufferedImageFromFile(path)
//    val before = System.currentTimeMillis()
//    bf.map(OperationF[IO].withPixelImage)
//      .map(image => image.output(JpegWriter.Default, "withLibrary.jpg"))
//      .map(_ => println(System.currentTimeMillis() - before))
//      .as(ExitCode.Success)
  }
}
