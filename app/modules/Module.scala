package modules

import actors.ScheduleActor
import actors.ScheduleActor.{RefreshGames, RefreshPrices, RefreshSteamGenresAndTags}
import akka.actor.{ActorRef, ActorSystem}
import com.google.inject.name.Named
import com.google.inject.{AbstractModule, Inject, Singleton}
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.duration.{FiniteDuration, MINUTES, SECONDS}

class Module extends AbstractModule with AkkaGuiceSupport {
  override def configure() = {
    bindActor[ScheduleActor]("ScheduleActor")
    bind(classOf[RefreshScheduler]).asEagerSingleton()
  }
}

@Singleton
class RefreshScheduler @Inject()(system: ActorSystem, @Named("ScheduleActor") scheduleActor: ActorRef) {
  system.scheduler.schedule(FiniteDuration(1, SECONDS), FiniteDuration(15, MINUTES), scheduleActor, RefreshGames())
  system.scheduler.schedule(FiniteDuration(15, SECONDS), FiniteDuration(120, MINUTES), scheduleActor, RefreshSteamGenresAndTags())
  system.scheduler.schedule(FiniteDuration(30, SECONDS), FiniteDuration(60, MINUTES), scheduleActor, RefreshPrices())
}