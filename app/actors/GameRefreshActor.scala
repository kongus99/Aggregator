package actors

import actors.GameRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.{Actor, ActorRef}
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
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
      val send: ActorRef = sender()
      refreshSteamGames().flatMap(_ => refreshGogGames()).onSuccess({case _ => sendRefreshed(send)})
  }

  private def sendRefreshed(send : ActorRef) = {
    GameEntry.gamesWithPrices(user.id.get, GameSources.WishList, tables)
      .zip(GameEntry.gamesWithPrices(user.id.get, GameSources.Owned, tables)).onSuccess({
      case (wishlist, owned) =>
          send ! UserGamesRefreshed(user.id.get, (wishlist, owned))
      })
  }

  private def refreshSteamGames(): Future[_] = {
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

  private def refreshGogGames(): Future[_] = {
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
