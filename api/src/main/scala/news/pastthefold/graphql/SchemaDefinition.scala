package news.pastthefold.graphql

import news.pastthefold.dao.SubscriptionState.SubscriptionState
import sangria.execution.deferred.{DeferredResolver, Fetcher, HasId}
import sangria.schema._

import cats._
import cats.implicits._

import scala.language.higherKinds

import scala.concurrent.Future


case class Storyline(id: String, slug: String, title: String, articleIds: List[String])
case class Article(id: String, uri: String, title: String)
object Article {
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
}

class ArticleDAOImpl[F[_] : Applicative] extends ArticleDAO[F] {
  val allArticles = List(
    Article("a11", "uri", "title"),
    Article("a12", "uri", "title"),
    Article("a21", "uri", "title"),
    Article("a22", "uri", "title"),
    Article("a31", "uri", "title"),
    Article("a32", "uri", "title"),
    Article("a41", "uri", "title"),
    Article("a42", "uri", "title"),
    Article("a51", "uri", "title"),
    Article("a52", "uri", "title"),
    Article("a61", "uri", "title"),
    Article("a62", "uri", "title"),
  )

  override def findAll(ids: Seq[String]): F[List[Article]] = {
    println(s"ArticleDAOImpl#findAll $ids")
    Applicative[F].pure(
      ids.map(id => allArticles.find(_.id == id))
        .flatten
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
    Storyline("s1", "slug1", "title1", List("a11", "a12")),
    Storyline("s2", "slug2", "title2", List("a21", "a22")),
    Storyline("s3", "slug3", "title3", List("a31", "a32")),
    Storyline("s4", "slug4", "title4", List("a41", "a42")),
    Storyline("s5", "slug5", "title5", List("a51", "a52")),
    Storyline("s6", "slug6", "title6", List("a61", "a62")),
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
    println(s"StorylineDAOImpl#findAll $offset $limit")
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

abstract class SchemaDefinition[F[_], QueryCtx <: QueryContext[F]] {
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
        resolve = ctx => articleFetcher.deferSeqOpt(ctx.value.articleIds))))

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
  val articleFetcher: Fetcher[QueryContext[F], Article, Article, String]
  def resolveStorylines(ctx: Context[QueryCtx, Unit], offset: Long, limit: Long): Action[QueryCtx, List[Storyline]]
  def resolveStoryline(ctx: Context[QueryCtx, Unit], id: String): Action[QueryCtx, Storyline]
}

class SchemaDefinitionFuture extends SchemaDefinition[Future, QueryContext[Future]] {

  def fetchArticles(ctx: QueryContext[Future], ids: Seq[String]): Future[List[Article]] =
    ctx.articleDAO.findAll(ids)

  // def fetchStorylines(ctx: StorylineDAO[Future], ids: Seq[String]): Future[List[Storyline]] =
  //   ctx.findAll(ids)

  // def fetchStorylines(ctx: StorylineDAO[Future], offset: Long, limit: Long): Future[List[Storyline]] =
  //   ctx.findAll(offset, limit)

  override val articleFetcher = Fetcher.caching(fetchArticles)

  def resolveStorylines(
                         ctx: Context[QueryContext[Future], Unit],
                         offset: Long,
                         limit: Long
                       ): FutureValue[QueryContext[Future], List[Storyline]] = {
    FutureValue[QueryContext[Future], List[Storyline]](
      ctx.ctx.storylineDAO.findAll(offset, limit))
  }

  def resolveStoryline(
                        ctx: Context[QueryContext[Future], Unit],
                        id: String
                      ): FutureValue[QueryContext[Future], Storyline] = {
    FutureValue[QueryContext[Future], Storyline](
      ctx.ctx.storylineDAO.find(id))
  }

  val resolver = DeferredResolver.fetchers(articleFetcher)
}

object SchemaDefinition {
  val schemaDefinition = new SchemaDefinitionFuture()
  val schema = schemaDefinition.buildSchema
}
