package actors

import actors.ScheduleActor.UserGamesRefreshed
import actors.SteamRefreshActor.RunRefresh
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.{ExecutionContext, Future}

object SteamRefreshActor {
  case class RunRefresh()
}

class SteamRefreshActor (user : User, client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val steamRetriever = new SteamPageRetriever(client)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  override def receive: Receive = {
    case _ : RunRefresh =>
      val s = sender()
      refreshSteamGames(user).onSuccess({case r => s ! UserGamesRefreshed(r)})
  }

  private def refreshSteamGames(user: User): Future[Option[JsValue]] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))
      wishlist <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))
      steamEntries = SteamEntry.parse(owned, wishlist, rates)
      _ <- tables.replaceSteamData(user, steamEntries)
    } yield {
      if(steamEntries.isEmpty) None else Some(Json.toJson(steamEntries))
    }
  }
}
