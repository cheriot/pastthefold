package news.pastthefold.graphql

import cats._
import cats.implicits._
import news.pastthefold.dao.SubscriptionState.SubscriptionState
import news.pastthefold.graphql.Article.ArticleId
import news.pastthefold.graphql.Storyline.StorylineId
import sangria.execution.deferred._
import sangria.schema._

import scala.concurrent.Future
import scala.language.higherKinds


case class Storyline(id: StorylineId, slug: String, title: String)
object Storyline {
  type StorylineId = String
  implicit val storylineHasId = HasId[Storyline, String](_.id)
}

case class Article(id: ArticleId, uri: String, title: String, storylineId: String)
object Article {
  type ArticleId = String
  implicit val articleHasId = HasId[Article, String](_.id)
}

case class Subscription(id: String, readerId: String, storylineId: String, state: SubscriptionState)
case class Reader(id: String, email: String)

object SubscriptionState extends Enumeration {
  type SubscriptionState = Value
  val UNCONFIRMED, ACTIVE, UNSUBSCRIBED = Value
}

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

  override def findAll(ids: Seq[String]): F[List[Article]] = {
    println(s"ArticleDAOImpl#findAll $ids")
    Applicative[F].pure(
      ids.map(id => allArticles.find(_.id == id))
        .flatten
        .toList
    )
  }

  override def findByStorylineIds(storylineIds: Seq[String]): F[List[Article]] = {
    println(s"ArticleDAOImpl#findByStorylineIds $storylineIds")
    Applicative[F].pure(
      storylineIds
        .flatMap(sId => allArticles.find(_.storylineId == sId))
        .toList
    )
  }
}

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

  override def find(id: String): F[Storyline] = {
    println(s"StorylineDAOImpl#find $id")
    findAll(Seq(id))
      .map(_.head)
  }

  override def findAll(ids: Seq[String]): F[List[Storyline]] = {
    println(s"StorylineDAOImpl#findAll $ids")
    Applicative[F].pure(
      ids.map(id => allStorylines.find(_.id == id))
        .flatten
        .toList
    )
  }

  override def findAll(offset: Long, limit: Long): F[List[Storyline]] = {
    Applicative[F].pure(
      allStorylines.slice(offset.toInt, (offset + limit).toInt)
    )
  }
}

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

/**
  * Better examples than the official docs.
  * https://github.com/howtographql/sangria/blob/master/src/main/scala/com/howtographql/scala/sangria/GraphQLSchema.scala
  *
  * @tparam F
  * @tparam QueryCtx
  */
abstract class SchemaDefinition[F[_], QueryCtx <: QueryContext[F]] {

  val articlesByStorylineRel = Relation[Article, StorylineId]("byStoryline", a => Seq(a.storylineId))

  lazy val StorylineType = ObjectType(
    "Storyline",
    "A series of articles that form a coherent line of investigation.",
    fields[ArticleDAO[F], Storyline](
      Field("id", StringType, Some("Uniquely identify a storyline."), resolve = _.value.id),
      Field("slug", StringType, Some("A more human friendly identifier unique among storylines."), resolve = _.value.slug),
      Field("title", StringType, Some("UI Text to label a storyline."), resolve = _.value.title),
      Field("articles",
        ListType(ArticleType),
        Some("Most recent articles added to the storyline."),
        resolve = ctx => articlesFetcher.deferRelSeq(articlesByStorylineRel, ctx.value.id))))

  lazy val ArticleType = ObjectType(
    "Article",
    "A published article.",
    fields[Unit, Article](
      Field("id", StringType, Some("Uniquely identify this article. Persistent across edits."), resolve = _.value.id),
      Field("uri", StringType, Some("Fully qualified URI where this article can be read."), resolve = _.value.uri),
      Field("title", StringType, Some("UI Text to label an article."), resolve = _.value.title)))

  val ID = Argument("id", StringType, description = "Unique identifier of this thing.")
  val LimitArg = Argument("limit", OptionInputType(LongType), defaultValue = 20)
  val OffsetArg = Argument("offset", OptionInputType(LongType), defaultValue = 0)

  val Query = ObjectType(
    "Query", fields[QueryCtx, Unit](
      Field(
        "storyline",
        StorylineType,
        arguments = ID :: Nil,
        resolve = (ctx) => resolveStoryline(ctx, ctx.arg(ID))),
      Field(
        "storylines",
        ListType(StorylineType),
        arguments = OffsetArg :: LimitArg :: Nil,
        resolve = (ctx) => resolveStorylines(ctx, ctx.arg(OffsetArg), ctx.arg(LimitArg)))))

  val buildSchema = Schema(Query)

  /*
   * Implement when a concrete F has been declared
   */
  val articlesFetcher: Fetcher[QueryContext[F], Article, Article, ArticleId]
  def resolveStorylines(ctx: Context[QueryCtx, Unit], offset: Long, limit: Long): Action[QueryCtx, List[Storyline]]
  def resolveStoryline(ctx: Context[QueryCtx, Unit], id: String): Action[QueryCtx, Storyline]
}

class SchemaDefinitionFuture extends SchemaDefinition[Future, QueryContext[Future]] {

  override val articlesFetcher: Fetcher[QueryContext[Future], Article, Article, ArticleId] = {
    def fetch(ctx: QueryContext[Future], ids: Seq[ArticleId]): Future[Seq[Article]] =
      ctx.articleDAO.findAll(ids)

    def fetchRel(ctx: QueryContext[Future], ids: RelationIds[Article]): Future[Seq[Article]] =
      ctx.articleDAO.findByStorylineIds(ids(articlesByStorylineRel))

    Fetcher.relCaching(fetch, fetchRel)
  }

  def resolveStorylines(
                         ctx: Context[QueryContext[Future], Unit],
                         offset: Long,
                         limit: Long
                       ): FutureValue[QueryContext[Future], List[Storyline]] = {
    FutureValue[QueryContext[Future], List[Storyline]](
      ctx.ctx.storylineDAO.findAll(offset, limit)
    )
  }

  def resolveStoryline(
                        ctx: Context[QueryContext[Future], Unit],
                        id: String
                      ): FutureValue[QueryContext[Future], Storyline] = {
    FutureValue[QueryContext[Future], Storyline](
      ctx.ctx.storylineDAO.find(id)
    )
  }

  val resolver = DeferredResolver.fetchers(articlesFetcher)
}

object SchemaDefinition {
  val schemaDefinition = new SchemaDefinitionFuture()
  val schema = schemaDefinition.buildSchema
}
