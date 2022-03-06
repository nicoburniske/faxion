package nicoburniske.faxion.image

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream, InputStream}
import java.net.URL

import cats.effect.IO
import cats.effect.kernel.Resource
import com.sksamuel.scrimage.ImmutableImage
import javax.imageio.ImageIO

object Loader {
  def imageFromFile(path: String): IO[ImmutableImage] = {
    getFileContentStream(path).use(r => IO(loadImageFromInputStream(r)))
  }

  def bufferedImageFromFile(path: String): IO[BufferedImage] = {
    getFileContentStream(path).use(r => IO(loadBufferedImageFromInputStream(r)))
  }

  def getUrlContentStream(url: String): Resource[IO, InputStream] = {
    Resource.make { IO.blocking(new URL(url).openConnection.getInputStream) } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def getFileContentStream(path: String): Resource[IO, InputStream] = {
    Resource.make { IO.blocking(new FileInputStream(new File(path))) } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def loadImageFromInputStream(stream: InputStream): ImmutableImage =
    ImmutableImage.loader().fromStream(stream)

  def loadBufferedImageFromInputStream(stream: InputStream): BufferedImage =
    ImageIO.read(stream)
}
