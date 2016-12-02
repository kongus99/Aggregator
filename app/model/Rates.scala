package model

import scala.xml.XML

case class Rates(euroTo: Map[String, Float]) {
  def recalculateFromEuro(valueInEuro: Float, currency: String): Option[Float] = {
    euroTo.get(currency).map(_ * valueInEuro)
  }
}

object Rates {
  def parseFromXml(xml: String): Rates = {
    val parsed = XML.loadString(xml)
    val rateMaps = (parsed \ "Cube" \ "Cube" \ "Cube").map(_.attributes.asAttrMap)
    Rates(rateMaps.map(e => (e("currency"), e("rate").toFloat)).toMap)
  }
}
