package modules

import akka.actor.{ActorRef, ActorSystem}
import com.google.inject.name.Named
import com.google.inject.{AbstractModule, Inject, Singleton}
import modules.ScheduleActor.refreshSchedule
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
  system.scheduler.schedule(FiniteDuration(1, SECONDS), FiniteDuration(2, MINUTES), scheduleActor, refreshSchedule)
}