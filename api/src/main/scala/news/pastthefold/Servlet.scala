package news.pastthefold

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.server.blaze._
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import org.http4s.servlet.{BlockingServletIo, Http4sServlet}
import news.pastthefold.HttpServices._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

class Servlet extends Http4sServlet[IO](
  helloWorldService,
  Duration.Inf,
  ExecutionContext.global,
  BlockingServletIo(Servlet.DefaultChunkSize),
  DefaultServiceErrorHandler
)

object Servlet {
  // From http4s/servlet/src/main/scala/org/http4s/servlet/package.scala
  val DefaultChunkSize = 4096
}