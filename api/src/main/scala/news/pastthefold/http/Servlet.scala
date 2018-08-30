package news.pastthefold.http

import cats.effect._
import news.pastthefold.http.HttpServices._
import org.http4s.server._
import org.http4s.servlet.{BlockingServletIo, Http4sServlet}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class Servlet extends Http4sServlet[IO](
  allHttpServices,
  2.seconds,
  ExecutionContext.global,
  BlockingServletIo[IO](Servlet.DefaultChunkSize),
  DefaultServiceErrorHandler
)

object Servlet {
  // From http4s/servlet/src/main/scala/org/http4s/servlet/package.scala
  val DefaultChunkSize = 4096
}