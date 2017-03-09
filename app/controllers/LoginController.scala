package controllers

import javax.inject.{Inject, Singleton}

import model.{Tables, User}
import org.jsoup.Jsoup
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, Controller}
import services.{GogWishListRetriever, PageRetriever, SteamPageRetriever}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LoginController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {


  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)

  def init = Action.async {
    Future {
      Ok(views.html.main("Aggregator - login", "javascripts/login", "Login"))
    }
  }

  def fetchBySteam(steamUsername: String) = Action.async {
    for{
      users <- tables.getBySteamLogin(steamUsername)
    } yield{
      Ok(Json.toJson(users))
    }
  }

  def fetchByGog(gogUsername: String) = Action.async {
    for{
      users <- tables.getByGogLogin(gogUsername)
    } yield{
      Ok(Json.toJson(users))
    }
  }

  def createUpdate(steamUsername: Option[String], gogUsername: Option[String]) = Action.async {
    def resolveUser(isValidSteam: Boolean, steamAlternate : Boolean, isValidGog: Boolean, steamExists: Option[User], gogExists: Option[User]) : Future[Option[User]] = {
      if (isValidGog && isValidGog)
        if (steamExists.isDefined && gogExists.isDefined)
          Future(None) //switch gog login between entries
        else
          tables.addUser(User(None, steamUsername, steamAlternate, gogUsername))
      else if (isValidSteam)
        steamExists.map(x => Future(Some(x))).getOrElse(tables.addUser(User(None, steamUsername, steamAlternate, None)))
      else if (isValidGog)
        gogExists.map(x => Future(Some(x))).getOrElse(tables.addUser(User(None, None, steamAlternate = false, gogUsername)))
      else
        Future(None)
    }

    val user = for{
      (isValidSteam, steamAlternate) <- isValidUsername(steamUsername, steamRetriever, "error_ctn")
      (isValidGog, _) <- isValidUsername(gogUsername, gogWishListRetriever, "error404")
      steamExists <- if(isValidSteam) tables.getSteamUser(steamUsername.get) else Future(None)
      gogExists <- if(isValidGog) tables.getGogUser(gogUsername.get) else Future(None)
      resolved <- resolveUser(isValidSteam, steamAlternate, isValidGog, steamExists, gogExists)
    } yield {
      resolved
    }
    user.map(u => {
      Ok(Json.toJson(u))
    })
  }



  private def isValidUsername(username: Option[String], retriever : PageRetriever, errorClass : String) : Future[(Boolean, Boolean)] = {
    def checkNoError(u: String, useAlternate : Boolean): Future[Boolean] = {
      for {
        ur <- retriever.retrieveWithUser(useAlternate)(u)("/wishlist")
      } yield {
        Jsoup.parse(ur).body().getElementsByClass(errorClass).size() <= 0
      }
    }

    username.map(u =>
      if (!u.isEmpty) {
        checkNoError(u, useAlternate = false).flatMap(r => if(r) Future((r, false)) else checkNoError(u, useAlternate = true).map((_, true)))
      } else Future((false, false))
    ).getOrElse(Future((false, false)))
  }

  def steamAlternate(userId: Long, steamAlternate: Boolean) = Action.async {
    tables.updateSteamAlternate(userId, steamAlternate).map(x => Ok(Json.toJson(x)))

  }
}
