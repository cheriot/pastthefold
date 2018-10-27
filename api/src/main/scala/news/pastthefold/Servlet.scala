package news.pastthefold

import cats.effect._
import news.pastthefold.graphql.GraphQLExecutor
import news.pastthefold.http.GraphQLHttpRoutes
import org.http4s.server._
import org.http4s.servlet.{BlockingServletIo, Http4sServlet}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class Servlet extends Http4sServlet[IO](
  Registry().allHttpEndpoints,
  Config.idleTimeout,
  ExecutionContext.global,
  BlockingServletIo[IO](Servlet.DefaultChunkSize),
  DefaultServiceErrorHandler
)

object Servlet {
  // From http4s/servlet/src/main/scala/org/http4s/servlet/package.scala
  val DefaultChunkSize = 4096
}