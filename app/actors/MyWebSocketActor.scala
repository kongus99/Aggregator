package actors

import actors.MyWebSocketActor.RefreshUserData
import akka.actor._
import play.api.libs.json.JsValue

object MyWebSocketActor {
  def props(out: ActorRef) = Props(new MyWebSocketActor(out))

  case class RefreshUserData(data : JsValue)

}

class MyWebSocketActor(out: ActorRef) extends Actor {
  def receive() = {
    case d : RefreshUserData =>
      out ! d.data
  }
}
