package news.pastthefold

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.server.blaze._
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import HttpServices._


object Main extends StreamApp[IO] {


  println("Main is running")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    BlazeBuilder[IO]
      .bindHttp(9090, "localhost")
      .mountService(helloWorldService, "/")
      .mountService(graphQLService, "/graphql")
      .serve
}