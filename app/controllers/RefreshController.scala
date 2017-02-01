package controllers

import javax.inject.Inject

import actors.MyWebSocketActor
import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.streams._
import play.api.mvc._

class RefreshController @Inject() (implicit system: ActorSystem, materializer: Materializer) {

  def socket = WebSocket.accept[String, String] { _ =>
    ActorFlow.actorRef(out => MyWebSocketActor.props(out))
  }
}