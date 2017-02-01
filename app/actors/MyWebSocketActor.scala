package actors

import actors.MyWebSocketActor.RefreshUserData
import akka.actor._

object MyWebSocketActor {
  def props(out: ActorRef) = Props(new MyWebSocketActor(out))

  case class RefreshUserData()

}

class MyWebSocketActor(out: ActorRef) extends Actor {
  def receive(): PartialFunction[Any, Unit] = {
    case _: RefreshUserData =>
      out ! "Please refresh data."
  }
}
