package news.pastthefold.dao

import cats._
import cats.implicits._
import news.pastthefold.model.Storyline

trait StorylineDAO[F[_]] {
  def find(id: String): F[Storyline]
  def findAll(ids: Seq[String]): F[List[Storyline]]
  def findAll(offset: Long, limit: Long): F[List[Storyline]]
}

class StorylineDAOImpl[F[_] : Applicative] extends StorylineDAO[F] {

  val allStorylines = List(
    Storyline("s1", "slug1", "title1"),
    Storyline("s2", "slug2", "title2"),
    Storyline("s3", "slug3", "title3"),
    Storyline("s4", "slug4", "title4"),
    Storyline("s5", "slug5", "title5"),
    Storyline("s6", "slug6", "title6"),
  )

  override def find(id: String): F[Storyline] =
    findAll(Seq(id))
      .map(_.head)

  override def findAll(ids: Seq[String]): F[List[Storyline]] =
    Applicative[F].pure(
      ids.map(id => allStorylines.find(_.id == id))
        .flatten
        .toList
    )

  override def findAll(offset: Long, limit: Long): F[List[Storyline]] =
    Applicative[F].pure(
      allStorylines.slice(offset.toInt, (offset + limit).toInt)
    )

}
