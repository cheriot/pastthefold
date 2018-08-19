package news.pastthefold.graphql

import cats.implicits._
import news.pastthefold.dao.{ArticleDAO, ArticleDAOImpl, StorylineDAO, StorylineDAOImpl}

import scala.concurrent.Future

trait QueryContext[F[_]] {
  def storylineDAO: StorylineDAO[F]
  def articleDAO: ArticleDAO[F]
}
object QueryContext {
  import scala.concurrent.ExecutionContext.Implicits.global
  val buildContext = new QueryContext[Future] {
    val storylineDAO = new StorylineDAOImpl[Future]
    val articleDAO = new ArticleDAOImpl[Future]
  }
}


