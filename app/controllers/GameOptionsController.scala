package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, AnyContent, Controller}
import services.PriceHost.{PriceHost => _, _}
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables, client: WSClient)(implicit exec: ExecutionContext) extends Controller {

  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)
  val golRetriever = new GolRetriever(client)

  def fetch(steamId: Long): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      queries <- queries(steamEntry, steamEntry.name, None)
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  def changeSearch(userId: Long, selectedResult: String, site: String, steamId: Long): Action[AnyContent] = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }

  def fetchSearch(userId: Long, query: String, site: String, steamId: Long): Action[AnyContent] = Action.async {

    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      queries <- queries(steamEntry, query, Some(site))
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  private def queries(steamEntry: SteamEntry, query: String, site: Option[String]): Future[Seq[GameQuery]] = {
    def getQuery(currentSite: String): String = site.filter(_ == currentSite).map(_ => query).getOrElse(steamEntry.name)

    for {
      keyeNames <- KeyePricesFetcher.getSuggestions(getQuery(Keye.toString), tables, keyeRetriever.retrieve)
      fkNames <- FKPricesFetcher.getSuggestions(getQuery(FK.toString), tables, fkRetriever.retrieve)
      golNames <- GolPricesFetcher.getSuggestions(getQuery(Gol.toString), tables, golRetriever.retrieve)
    } yield {
      GameQuery(getQuery(FK.toString), FK.toString, fkNames.take(5), "") ::
        GameQuery(getQuery(Keye.toString), Keye.toString, keyeNames.take(5), "") ::
        GameQuery(getQuery(Gol.toString), Gol.toString, golNames.take(5), "") ::
        Nil
    }
  }

}