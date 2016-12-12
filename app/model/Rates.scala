package model

import scala.xml.XML

case class Rates(euroTo: Map[String, BigDecimal]) {
  def recalculateFromEuro(valueInEuro: BigDecimal, currency: String): Option[BigDecimal] = {
    euroTo.get(currency).map(_ * valueInEuro)
  }
}

object Rates {
  def parseFromXml(xml: String): Rates = {
    val parsed = XML.loadString(xml)
    val rateMaps = (parsed \ "Cube" \ "Cube" \ "Cube").map(_.attributes.asAttrMap)
    Rates(rateMaps.map(e => (e("currency"), BigDecimal(e("rate")))).toMap)
  }
}
