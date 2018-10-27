package news.pastthefold

import cats.effect._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import org.http4s.HttpService
import org.http4s.server.blaze._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StreamApp[IO] {

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    for {
      exitCode <- createStream[IO](Registry().allHttpEndpoints)
    } yield exitCode
  }

  def createStream[F[_]: Effect](
                                  authEndpoings: HttpService[F]
                                ): Stream[F, ExitCode] =
    BlazeBuilder[F]
      .bindHttp(9090, "localhost")
      .mountService(authEndpoings, "/")
      .withIdleTimeout(Config.idleTimeout)
      .serve
}