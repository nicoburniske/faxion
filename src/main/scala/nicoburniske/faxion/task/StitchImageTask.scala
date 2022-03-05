package nicoburniske.faxion.task

import java.io.InputStream

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.parallel._
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import nicoburniske.faxion.image.Loader

object StitchImageTask extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val pantsUrl = "https://cdn.fs.grailed.com/api/file/GhiQyKrS456JVwOYLnQD"
    loadImages(Seq(pantsUrl)).map(_.head).map(_.output(JpegWriter.Default, "test.jpeg")).as(ExitCode.Success)
  }

  def loadImages(imageUrls: Seq[String]): IO[Seq[ImmutableImage]] = {
    val parallel: Resource[IO, Seq[InputStream]] = imageUrls.map(Loader.getUrlContentStream).parSequence
    parallel.use { inputStreams => IO(inputStreams.map(Loader.loadImageFromInputStream)) }
  }
}
