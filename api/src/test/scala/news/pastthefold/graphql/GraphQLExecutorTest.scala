package news.pastthefold.graphql

import java.nio.charset.StandardCharsets

import cats.effect.IO
import io.circe.parser._
import org.http4s.{Response, Status}
import utest._


object GraphQLExecutorTest extends TestSuite {
  val tests = Tests {
    "Query schema types" - {
      val query = """
        |{
        |  __schema {
        |    types {
        |      name
        |    }
        |  }
        |}
      """.stripMargin
      val response = sendQuery(query)
      assert(response.status == Status.Ok)
      val responseBody = body(response)
      println(parse(responseBody).right.get.spaces2)
    }

    "Query storylines" - {
      val query =
        """
          |{
          |  storylines {
          |    id, title, articles { id }
          |  }
          |}
        """.stripMargin
      val response = sendQuery(query)
      val responseBody = body(response)
      println(parse(responseBody).right.get.spaces2)
    }
  }

  def body(response: Response[IO]): String =
    new String(response.body.compile.toVector.unsafeRunSync().toArray, StandardCharsets.UTF_8)

  def sendQuery(query: String): Response[IO] = {
    println(query)
    val json =
      s"""
         |{
         |  "query": "${query.replaceAll("\n", "").trim}"
         |}
        """.stripMargin
    GraphQLExecutor().httpGraphQL(json).unsafeRunSync()
  }


}
