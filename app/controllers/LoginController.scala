package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.Configuration
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

  def fetch(steamUsername: Option[String], gogUsername: Option[String]) = {
    play.mvc.Results.TODO
  }

  def createUpdate(steamUsername: Option[String], gogUsername: Option[String]) = {
    play.mvc.Results.TODO
  }
}
