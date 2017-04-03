package actors

import actors.MyWebSocketActor.RefreshUserData
import akka.actor._
import play.api.libs.json.JsValue

object MyWebSocketActor {
  def props(userId : Long, out: ActorRef) = Props(new MyWebSocketActor(userId, out))

  case class RefreshUserData(userId : Long, data : JsValue)

}

class MyWebSocketActor(userId : Long, out: ActorRef) extends Actor {
  def receive() = {
    case d : RefreshUserData =>
      if(userId == d.userId) out ! d.data
  }
}
