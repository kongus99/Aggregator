package actors

import actors.SteamRefreshActor.RunRefresh
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.duration.{FiniteDuration, MINUTES}
import scala.concurrent.{Await, ExecutionContext, Future}

object SteamRefreshActor {
  case class RunRefresh(user : User)
}

class SteamRefreshActor (client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val steamRetriever = new SteamPageRetriever(client)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  override def receive: Receive = {
    case r : RunRefresh =>
      Await.result(refreshSteamGames(r.user), FiniteDuration(5, MINUTES))
      println("refreshed steam user " + r.user)
  }

  private def refreshSteamGames(user: User): Future[Any] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))
      wishlist <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))
      refreshed <- tables.replaceSteamData(user, SteamEntry.parse(owned, wishlist, rates))
    } yield {
      refreshed
    }
  }
}
