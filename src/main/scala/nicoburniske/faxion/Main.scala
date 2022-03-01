package nicoburniske.faxion

import java.io.File

import cats.effect.IO
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter

object Main {
  def main(args: Array[String]): Unit = {
    val image   = ImmutableImage.loader().fromFile("example/fit1/pants.jpeg")
    val flipped = image.flipY()
    flipped.output(JpegWriter.Default, "flipped.jpeg")
  }

  def findImages(path: String): IO[Seq[File]] = {
    val directory = new File(path)
    if (directory.exists && directory.isDirectory) {
      IO {
        directory.listFiles.filter(isImage)
      }
    } else {
      IO.raiseError(new IllegalArgumentException("Invalid directory path"))
    }
  }

  val IMAGE_EXT = Set("jpg", "jpeg", "png")

  def isImage(file: File): Boolean = {
    Option(file)
      .filter(_.exists)
      .filter(_.isFile)
      .map(_.getName)
      .filter(_.contains('.'))
      .map { s => s.substring(s.lastIndexOf(".") + 1) }
      .exists(IMAGE_EXT)
  }
}
