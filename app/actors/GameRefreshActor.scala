package actors

import actors.GameRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.{ExecutionContext, Future}

object GameRefreshActor {
  case class RunRefresh()
}

class GameRefreshActor (user : User, client: WSClient, tables: Tables, configuration: Configuration, implicit val exec: ExecutionContext) extends Actor {
  val gogRetriever = new GogPageRetriever(client, configuration)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  override def receive: Receive = {
    case _ : RunRefresh =>
      val send = sender()
      refreshSteamGames(user)
        .flatMap(_ => refreshGogGames(user))
        .flatMap(_ => GameEntry.generateFromNames(Some(user), GameSources.WishList, tables))
        .onSuccess({case list => send ! UserGamesRefreshed(Some(Json.toJson(list)))})
  }

  private def refreshSteamGames(user: User): Future[_] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))
      wishlist <- user.steamLogin.map(l => steamRetriever.retrieveWithUser(user.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))
      steamEntries = SteamEntry.parse(owned, wishlist, rates)
      replaced <- tables.replaceSteamData(user, steamEntries)
    } yield {
      replaced
    }
  }

  private def refreshGogGames(user: User): Future[_] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))
      gogEntries = GogEntry.parse(owned, wishlist, rates)
      replaced <- tables.replaceGogData(user, gogEntries)
    } yield {
      replaced
    }
  }
}
