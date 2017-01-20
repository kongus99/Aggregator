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

  def fetch(steamUsername: Option[String], gogUsername: Option[String]) = Action.async {
    for{
      u <- tables.getUserByLogin(steamUsername, gogUsername)
    } yield{
      Ok(Json.toJson(u))
    }
  }

  def createUpdate(steamUsername: Option[String], steamAlternate : Boolean, gogUsername: Option[String]) = Action.async {
    def resolveUser(isValidSteam: Boolean, isValidGog: Boolean, steamExists: Option[User], gogExists: Option[User]) : Future[Option[User]] = {
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
      isValidSteam <- isValidUsername(steamUsername, steamAlternate, steamRetriever, "error_ctn")
      isValidGog <- isValidUsername(gogUsername, useAlternateAddress = false, gogWishListRetriever, "error404")
      steamExists <- if(isValidSteam) tables.getSteamUser(steamUsername.get) else Future(None)
      gogExists <- if(isValidGog) tables.getGogUser(gogUsername.get) else Future(None)
      resolved <- resolveUser(isValidSteam, isValidGog, steamExists, gogExists)
    } yield {
      resolved
    }
    user.map(u => {
      Ok(Json.toJson(u))
    })
  }



  private def isValidUsername(username: Option[String], useAlternateAddress : Boolean, retriever : PageRetriever, errorClass : String) = {
    username.map(u =>
      if (!u.isEmpty) {
        for {
          ur <- retriever.retrieveWithUser(useAlternateAddress)(u)("/wishlist")
        } yield {
          Jsoup.parse(ur).body().getElementsByClass(errorClass).size() <= 0
        }
      } else Future(false)
    ).getOrElse(Future(false))
  }

  def steamAlternate(userId: Long, steamAlternate: Boolean) = Action.async {
    tables.updateSteamAlternate(userId, steamAlternate).map(x => Ok(Json.toJson(x)))

  }
}
