package model

import play.api.libs.json.{JsPath, Writes}

case class User(id : Option[Long], steamLogin : Option[String], steamAlternate : Boolean, gogLogin : Option[String]){
  if((steamLogin.isEmpty || steamLogin.get.isEmpty) && (gogLogin.isEmpty || gogLogin.get.isEmpty))
    throw new DataError("One login must be defined")
}
object User {

    import play.api.libs.functional.syntax._

    implicit val steamWrites: Writes[User] = (
      (JsPath \ "id").write[Option[Long]] and
        (JsPath \ "steamLogin").write[Option[String]] and
        (JsPath \ "steamAlternate").write[Boolean] and
        (JsPath \ "gogLogin").write[Option[String]]) ((e) => (e.id, e.steamLogin, e.steamAlternate, e.gogLogin))
}
class DataError(message : String) extends Exception(message)
