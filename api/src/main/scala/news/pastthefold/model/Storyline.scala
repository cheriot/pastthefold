package news.pastthefold.model

import news.pastthefold.model.Storyline.StorylineId
import sangria.execution.deferred.HasId

case class Storyline(id: StorylineId, slug: String, title: String)
object Storyline {
  type StorylineId = String
  implicit val storylineHasId = HasId[Storyline, String](_.id)
}
