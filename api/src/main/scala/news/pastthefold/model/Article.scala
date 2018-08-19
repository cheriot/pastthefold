package news.pastthefold.model

import news.pastthefold.model.Article.ArticleId
import sangria.execution.deferred.HasId

case class Article(id: ArticleId, uri: String, title: String, storylineId: String)
object Article {
  type ArticleId = String
  implicit val articleHasId = HasId[Article, String](_.id)
}
