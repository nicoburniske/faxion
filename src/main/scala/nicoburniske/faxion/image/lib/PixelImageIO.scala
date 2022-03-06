package nicoburniske.faxion.image.lib

import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream, IOException, InputStream, OutputStream}

import javax.imageio.stream.MemoryCacheImageOutputStream
import javax.imageio.{IIOImage, ImageWriteParam}

import scala.util.Try

object PixelImageIO {

  /**
   * read image from stream
   */
  def readFromStream[Pixel](inputStream: InputStream)(
      implicit converter: BufferedImageConverter[Pixel]): Try[PixelImage[Pixel]] = Try {
    val img = javax.imageio.ImageIO.read(inputStream)
    converter.toPixelImage(img)
  }

  /**
   * write image to a stream, format needs to be specified
   * @param format
   *   format suffix, e.g. "png", "jpg"
   */
  def writeToStream[Pixel](image: PixelImage[Pixel], outputStream: OutputStream, format: String = "png")(
      implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = Try {
    val bufImage = converter.toBufferedImage(image)
    javax.imageio.ImageIO.write(bufImage, format, outputStream)
  }

  /**
   * read image from a file
   */
  def read[Pixel](file: File)(implicit converter: BufferedImageConverter[Pixel]): Try[PixelImage[Pixel]] =
    Try {
      val img = javax.imageio.ImageIO.read(file)
      converter.toPixelImage(img)
    }

  /**
   * write image to a file
   */
  def write[Pixel](image: PixelImage[Pixel], file: File)(
      implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = Try {
    val bufImage = converter.toBufferedImage(image)
    file match {
      case f if f.getAbsolutePath.toLowerCase.endsWith(".png") =>
        javax.imageio.ImageIO.write(bufImage, "png", file)
      case f if f.getAbsolutePath.toLowerCase.endsWith(".jpg") =>
        writeJPEGToStream(bufImage, new FileOutputStream(file), 1.0f)
      case _                                                   => throw new IOException("Unknown image format: " + file.getName)
    }
  }

  /**
   * Write a JPEG image with a specified quality level where 1.0 is the maximally possible and 0 the minimal
   * quality.
   */
  def writeJPEGToStream[Pixel](image: PixelImage[Pixel], outputStream: OutputStream, quality: Float)(
      implicit converter: BufferedImageConverter[Pixel]): Try[Unit] = Try {
    val img = converter.toBufferedImage(image)
    writeJPEGToStream(img, outputStream, quality)
  }

  /**
   * Write a JPEG image with a specified quality level where 1.0 is the maximally possible and 0 the minimal
   * quality.
   */
  def writeJPEGToStream[Pixel](image: BufferedImage, outputStream: OutputStream, quality: Float): Try[Unit] =
    Try {
      val jpgWriter                  = javax.imageio.ImageIO.getImageWritersByFormatName("jpg").next()
      val jpgParams: ImageWriteParam = jpgWriter.getDefaultWriteParam
      jpgParams.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
      jpgParams.setCompressionQuality(quality.toFloat)
      val imageStream                = new MemoryCacheImageOutputStream(outputStream)
      jpgWriter.setOutput(imageStream)
      jpgWriter.write(null, new IIOImage(image, null, null), jpgParams)
      jpgWriter.dispose()
    }
}
