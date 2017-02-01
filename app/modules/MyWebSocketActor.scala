package modules

import akka.actor._
import modules.MyWebSocketActor.RefreshUserData

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
