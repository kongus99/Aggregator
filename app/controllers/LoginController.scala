package controllers

import javax.inject.{Inject, Singleton}

import model.{Tables, User}
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, Controller}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LoginController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  def init = Action.async {
    Future {
      Ok(views.html.main("Aggregator - login", "javascripts/login", "Login"))
    }
  }

  def fetch(steamUsername: Option[String], gogUsername: Option[String]) = Action.async {
    for{
      u <- tables.getUserByLogin(User(None, steamUsername, gogUsername))
    } yield{
      Ok(Json.toJson(u))
    }
  }

  def createUpdate(steamUsername: Option[String], gogUsername: Option[String]) = {
    play.mvc.Results.TODO
  }
}
