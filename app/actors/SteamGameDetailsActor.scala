package actors

import actors.SteamGameDetailsActor.RunRefresh
import akka.actor.Actor
import model.Tables
import play.api.libs.ws.WSClient
import services.{SteamEntry, SteamStorePageRetriever}

import scala.concurrent.{ExecutionContext, Future}


object SteamGameDetailsActor {

  case class RunRefresh()

}

class SteamGameDetailsActor(steamIds: Seq[Long], client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val steamRetriever = new SteamStorePageRetriever(client)

  override def receive: Receive = {
    {
      case _: RunRefresh =>
        val categoriesAndTags = for {
          detailPages <- Future.sequence(steamIds.map(id => steamRetriever.retrieve(id.toString).map((id, _))))
        } yield {
          detailPages.map({
            case (steamId: Long, page: String) =>
              val parsed = SteamEntry.parseGenresAndTags(page)
              (steamId, parsed._1, parsed._2)
          })
        }
        categoriesAndTags.map(tables.updateSteamGenresAndTags)
    }
  }
}
