package controllers

import javax.inject._

import model.{Tables, User}
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry.{generateFromNames, _}
import services.GameSources.GameSources
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MainController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }

  private def getGogGames(user: Option[User]) = {
    for {
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
    } yield {
      (owned, wishlist)
    }
  }

  private def getSteamGames(user: Option[User]) = {
    for {
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))).getOrElse(Future(""))
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
    } yield {
      (owned, wishlist)
    }
  }

  def getUserGames(userId: Long, sources: GameSources): Action[AnyContent] = Action.async {
    for {
      user <- tables.getUserById(userId)
      result <- generateFromNames(user, sources, tables)
      prices <- tables.getPrices(result.map(_.steam).filter(_.nonEmpty).flatten).map(_.groupBy(_.steamId).mapValues(_.sortBy(_.price)))
    } yield {
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))))
    }
  }

  def getGameOptions(gameId: Long): Action[AnyContent] = Action.async {
    import play.api.libs.functional.syntax._
    import play.api.libs.json.{JsPath, Writes}

    case class GameQuery(query: String, site: String, results: List[String], selectedResult : Int)
    case class GameOptions(entry: SteamEntry, queries: List[GameQuery])

    implicit val gameQueryWrites: Writes[GameQuery] =
      ((JsPath \ "query").write[String] and
       (JsPath \ "site").write[String] and
       (JsPath \ "results").write[Seq[String]] and
       (JsPath \ "selectedResult").write[Int] ) ((q) => (q.query, q.site, q.results, q.selectedResult))

    implicit val gameOptionsWrites: Writes[GameOptions] =
      ((JsPath \ "entry").write[SteamEntry] and
       (JsPath \ "queries").write[Seq[GameQuery]]) ((o) => (o.entry, o.queries))

    val st = SteamEntry("Some Game", gameId, None, None, owned = true)
    val go = GameOptions(st, GameQuery("some query", "some site", "result1" :: "result2" :: Nil, 0) :: Nil)
    Future(Ok(Json.toJson(go)))
  }

}
