package binders

import play.api.mvc.QueryStringBindable
import services.GameOn
import services.GameOn.GameOn

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
}
