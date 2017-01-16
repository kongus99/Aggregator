package model

import scala.xml.XML

case class CurrencyConverter(euroTo: Map[String, BigDecimal]) {

  def convert(value: String): Option[BigDecimal] = {
    try {
      if (value.contains('€'))
        recalculate(toBigDecimal(value, "€"), "EUR", "PLN")
      else if (value.contains('$'))
        recalculate(toBigDecimal(value, "$"), "USD", "PLN")
      else
        recalculate(toBigDecimal(value, "zł"), "PLN", "PLN")
    } catch {
      case _: NumberFormatException => None
    }
  }

  private def toBigDecimal(value: String, currencySeparator: String): BigDecimal = {
    BigDecimal(value.replace(currencySeparator, "").trim.replace(",", "."))
  }

  private def recalculate(value: BigDecimal, currencyFrom : String, currencyTo: String): Option[BigDecimal] = {
    if(currencyFrom.equals(currencyTo))
      Some(value)
    else if(currencyFrom.equals("EUR"))
      euroTo.get(currencyTo).map(_ * value)
    else
      for{
        toEuro <- euroTo.get(currencyFrom)
        toTarget <- euroTo.get(currencyTo)
      } yield (value / toEuro) * toTarget
  }
}

object CurrencyConverter {
  def parseFromXml(xml: String): CurrencyConverter = {
    val parsed = XML.loadString(xml)
    val rateMaps = (parsed \ "Cube" \ "Cube" \ "Cube").map(_.attributes.asAttrMap)
    CurrencyConverter(rateMaps.map(e => (e("currency"), BigDecimal(e("rate")))).toMap)
  }
}
