package modules

import akka.actor.{Actor, ActorSystem, Props}
import com.google.inject.Inject
import model.{CurrencyConverter, Tables}
import modules.PriceRefreshActor.RunRefresh
import modules.ScheduleActor.{RefreshGog, RefreshPrices, RefreshSteam}
import play.api.Configuration
import play.api.libs.ws.WSClient
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.duration.{FiniteDuration, MINUTES, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

object ScheduleActor {

  case class RefreshSteam()

  case class RefreshGog()

  case class RefreshPrices()

}

class ScheduleActor @Inject()(system : ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {
  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  override def receive: Receive = {
    case _: RefreshSteam =>
      Await.result(refreshSteamGames(1L), FiniteDuration(5, MINUTES))
      println("Refreshed steam")
    case _: RefreshGog =>
      Await.result(refreshGogGames(1L), FiniteDuration(5, MINUTES))
      println("Refreshed gog")
    case _: RefreshPrices =>
      Await.result(refreshPrices(), FiniteDuration(5, MINUTES))
      println("refreshed prices")
  }

  private def refreshGogGames(userId: Long): Future[Any] = {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
      refreshed <- tables.replaceGogData(user, GogEntry.parse(owned, wishlist, rates))
    } yield {
      refreshed
    }
  }

  private def refreshSteamGames(userId: Long): Future[Any] = {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))).getOrElse(Future(""))
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
      refreshed <- tables.replaceSteamData(user, SteamEntry.parse(owned, wishlist, rates))

    } yield {
      refreshed
    }
  }

  private def refreshPrices(): Future[Unit] = {
    val actors = (1 to 2).map(_ => system.actorOf(Props(classOf[PriceRefreshActor], client, tables, exec))).toArray
    def scheduleRefresh(e : Seq[SteamEntry], i : Int) : Unit = system.scheduler.scheduleOnce(FiniteDuration(1, SECONDS), actors(i % actors.length), RunRefresh(e))
    tables.getSteamEntries(None, None).map(entries => entries.grouped(20).toSeq.zipWithIndex.foreach((scheduleRefresh _).tupled))
  }
}