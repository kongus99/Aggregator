package binders

import play.api.mvc.QueryStringBindable
import services.GameOn.GameOn
import services.GameSources.GameSources
import services.{GameOn, GameSources}

object CustomBinders {
  implicit def gameOnBindable(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[GameOn] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, GameOn]] =
      for {
        on <- stringBinder.bind(key, params)
      } yield {
        on.right.map(x => GameOn.withName(x))
      }

    override def unbind(key: String, on: GameOn): String = stringBinder.unbind(key, on.toString)
  }

  implicit def gameSourcesBindable(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[GameSources] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, GameSources]] =
      for {
        on <- stringBinder.bind(key, params)
      } yield {
        on.right.map(x => GameSources.withName(x))
      }

    override def unbind(key: String, on: GameSources): String = stringBinder.unbind(key, on.toString)
  }
}
