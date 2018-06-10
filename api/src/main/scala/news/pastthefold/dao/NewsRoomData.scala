package news.pastthefold.dao

import news.pastthefold.dao.SubscriptionState.SubscriptionState

case class Article(id: String, uri: String, title: String)
case class Storyline(id: String, title: String)
case class Subscription(id: String, readerId: String, storylineId: String, state: SubscriptionState)
case class Reader(id: String, email: String)

object SubscriptionState extends Enumeration {
  type SubscriptionState = Value
  val UNCONFIRMED, ACTIVE, UNSUBSCRIBED = Value
}

trait NewsRoomRepo {

}
class NewsRoomData {

}
