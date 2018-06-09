package news.pastthefold

import cats.effect._
import news.pastthefold.HttpServices._
import org.http4s.server._
import org.http4s.servlet.{BlockingHttp4sServlet, BlockingServletIo}

class Servlet extends BlockingHttp4sServlet[IO](
  allHttpServices,
  BlockingServletIo(Servlet.DefaultChunkSize),
  DefaultServiceErrorHandler
)

object Servlet {
  // From http4s/servlet/src/main/scala/org/http4s/servlet/package.scala
  val DefaultChunkSize = 4096
}