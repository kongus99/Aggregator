package model

case class User(id : Option[Long], steamLogin : Option[String], gogLogin : Option[String]){
  if(steamLogin.isEmpty && gogLogin.isEmpty)
    throw new DataError("One login must be defined")
}

class DataError(message : String) extends Exception(message)
