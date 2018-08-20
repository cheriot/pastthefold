package news.pastthefold.graphql

import news.pastthefold.model.Article.ArticleId
import news.pastthefold.model.{Article, Storyline}
import news.pastthefold.model.Storyline.StorylineId
import sangria.execution.deferred._
import sangria.schema._

import scala.concurrent.Future

/**
  * Better examples than the official docs.
  * https://github.com/howtographql/sangria/blob/master/src/main/scala/com/howtographql/scala/sangria/GraphQLSchema.scala
  *
  */
abstract class StorylineSchemaDefinition[F[_]] {

  val articlesByStorylineRel = Relation[Article, StorylineId]("byStoryline", a => Seq(a.storylineId))

  lazy val StorylineType = ObjectType(
    "Storyline",
    "A series of articles that form a coherent line of investigation.",
    fields[Unit, Storyline](
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
    "Query",
    fields[QueryContext[F], Unit](
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

  lazy val resolver = DeferredResolver.fetchers(articlesFetcher)
  val schema = Schema(Query)

  /*
   * Implement with a concrete F
   */
  val articlesFetcher: Fetcher[QueryContext[F], Article, Article, ArticleId]

  def resolveStorylines(ctx: Context[QueryContext[F], Unit], offset: Long, limit: Long ): Action[QueryContext[F], List[Storyline]]

  def resolveStoryline(ctx: Context[QueryContext[F], Unit], id: String): Action[QueryContext[F], Storyline]
}

class StorylineSchemaDefinitionFuture extends StorylineSchemaDefinition[Future] {

  val articlesFetcher: Fetcher[QueryContext[Future], Article, Article, ArticleId] = {
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
                       ): FutureValue[QueryContext[Future], List[Storyline]] =
    FutureValue(
      ctx.ctx.storylineDAO.findAll(offset, limit)
    )

  def resolveStoryline(
                        ctx: Context[QueryContext[Future], Unit],
                        id: String
                      ): FutureValue[QueryContext[Future], Storyline] =
    FutureValue(
      ctx.ctx.storylineDAO.find(id)
    )
}

object SchemaDefinition {
  val storylineSchemaDefinition = new StorylineSchemaDefinitionFuture
}
