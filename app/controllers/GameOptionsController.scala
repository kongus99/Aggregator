package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, AnyContent, Controller}
import services.PriceHost._
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables, client: WSClient)(implicit exec: ExecutionContext) extends Controller {

  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)
  val golRetriever = new GolRetriever(client)

  def fetch(userId: Long, steamId: Long): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      definedQueries <- tables.getQueryData(userId, steamId)
      queries <- queries(steamEntry, definedQueries)
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  def changeSearch(userId: Long, steamId: Long, site: String, selectedResult: Option[String]): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      _ <- tables.changeQueryData(userId, steamId, GameQuery(steamEntry.name, site, Seq(), selectedResult), Right(selectedResult))
    } yield {
      Ok(Json.toJson(""))
    }
  }

  def fetchSearch(userId: Long, steamId: Long, query: String, site: String): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      _ <- tables.changeQueryData(userId, steamId, GameQuery(query, site, Seq(), None), Left(query))
      definedQueries <- tables.getQueryData(userId, steamId)
      queries <- queries(steamEntry, definedQueries)
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  private def queries(steamEntry: SteamEntry, definedQueries : Seq[GameQuery]): Future[Seq[GameQuery]] = {
    val queriesMap = definedQueries.map(q => (q.site, q)).toMap

    def getQuery(currentSite: String): String = queriesMap.get(currentSite).map(_.query).getOrElse(steamEntry.name)

    def createQuery(currentSite: String, results : Seq[String]) : GameQuery =
      queriesMap.get(currentSite).map(_.copy(allResults = results)).getOrElse(GameQuery(getQuery(currentSite), currentSite, results, None))

    for {
      keyeNames <- KeyePricesFetcher.getSuggestions(getQuery(Keye.toString), tables, keyeRetriever.retrieve)
      fkNames <- FKPricesFetcher.getSuggestions(getQuery(FK.toString), tables, fkRetriever.retrieve)
      golNames <- GolPricesFetcher.getSuggestions(getQuery(Gol.toString), tables, golRetriever.retrieve)
    } yield {
      createQuery(FK.toString, fkNames.map(_.name)) :: createQuery(Keye.toString, keyeNames.map(_.name)) :: createQuery(Gol.toString, golNames.map(_.name)) :: Nil
    }
  }

}