package news.pastthefold

import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

object HttpServices {
  val helloWorldService = HttpService[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }
}
