package news.pastthefold.dao

import cats._
import news.pastthefold.model.Article

trait ArticleDAO[F[_]] {
  def findAll(ids: Seq[String]): F[List[Article]]
  def findByStorylineIds(ids: Seq[String]): F[List[Article]]
}

class ArticleDAOImpl[F[_] : Applicative] extends ArticleDAO[F] {
  val allArticles = List(
    Article("a11", "uri", "title", "s1"),
    Article("a12", "uri", "title", "s1"),
    Article("a21", "uri", "title", "s2"),
    Article("a22", "uri", "title", "s2"),
    Article("a31", "uri", "title", "s3"),
    Article("a32", "uri", "title", "s3"),
    Article("a41", "uri", "title", "s4"),
    Article("a42", "uri", "title", "s4"),
    Article("a51", "uri", "title", "s5"),
    Article("a52", "uri", "title", "s5"),
    Article("a61", "uri", "title", "s6"),
    Article("a62", "uri", "title", "s6"),
  )

  override def findAll(ids: Seq[String]): F[List[Article]] =
    Applicative[F].pure(
      ids.map(id => allArticles.find(_.id == id))
        .flatten
        .toList
    )

  override def findByStorylineIds(storylineIds: Seq[String]): F[List[Article]] =
    Applicative[F].pure(
      storylineIds
        .flatMap(sId => allArticles.find(_.storylineId == sId))
        .toList
    )
}
