package news.pastthefold

import cats.effect._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import news.pastthefold.http.HttpServices._
import org.http4s.server.blaze._

import scala.concurrent.ExecutionContext.Implicits.global


object Main extends StreamApp[IO] {

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    BlazeBuilder[IO]
      .bindHttp(9090, "localhost")
      .mountService(allHttpServices, "/")
      .serve
}