package nicoburniske.faxion

import cats.Parallel
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.syntax.all._
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.JpegWriter
import fs2.{Chunk, Pipe, Stream}
import nicoburniske.faxion.image.OperationF
import org.http4s.EntityDecoder.multipart
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.http4s.multipart.Part
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.{HttpRoutes, MediaType}

import scala.concurrent.duration._

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    ServerApp[IO].stream.compile.drain.as(ExitCode.Success)
}

class ServerApp[F[_]: Async: Parallel] extends Http4sDsl[F] {
  def routes(wsb: WebSocketBuilder2[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "hello"         =>
        Ok("Hello world.")
      case req @ POST -> Root / "stitch" =>
        req.decodeWith(multipart[F], strict = true) { multipart =>
          val images: Seq[F[ImmutableImage]] =
            multipart.parts.sortWith(_.name < _.name).map { part: Part[F] =>
              part.body.compile.to(Array).map(ImmutableImage.loader().fromBytes(_))
            }
          for {
            _        <- multipart.parts.map(_.toString).foreach(println).pure
            stitched <- OperationF[F].stitchImages(images)
            bytes     = stitched.bytes(JpegWriter.Default)
            stream    = Stream.chunk(Chunk.array(bytes))
            response <- Ok("images stitched successfully")
          } yield response
            .withBodyStream(stream)
            .withContentType(`Content-Type`(MediaType.image.jpeg))
            .putHeaders(`Content-Length`(bytes.length))
        }
      case GET -> Root / "ws"            =>
        val toClient: Stream[F, WebSocketFrame]       =
          Stream.awakeEvery[F](1.seconds).flatMap(d => Stream.apply(Text(s"Ping! $d"), Text(s"Yeet!")))
        val fromClient: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) => Async[F].delay(println("response received " + t))
          case _          => Async[F].delay(println("Received something else?!"))
        }
        wsb.build(toClient, fromClient)
    }

  def stream: Stream[F, ExitCode] =
    BlazeServerBuilder[F].bindHttp(8080).withHttpWebSocketApp(routes(_).orNotFound).serve
}
object ServerApp {
  def apply[F[_]: Async: Parallel]: ServerApp[F] = new ServerApp[F]
}
