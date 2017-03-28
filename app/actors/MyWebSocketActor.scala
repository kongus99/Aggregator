package actors

import actors.MyWebSocketActor.RefreshUserGames
import akka.actor._
import play.api.libs.json.JsValue

object MyWebSocketActor {
  def props(userId : Long, out: ActorRef) = Props(new MyWebSocketActor(userId, out))

  case class RefreshUserGames(userId : Long, data : JsValue)

}

class MyWebSocketActor(userId : Long, out: ActorRef) extends Actor {
  def receive() = {
    case d : RefreshUserGames =>
      if(userId == d.userId) out ! d.data
  }
}
