package model

import org.slf4j.LoggerFactory

import scala.xml.XML

case class CurrencyConverter(euroTo: Map[String, BigDecimal]) {
  val logger = LoggerFactory.getLogger("chapters.introduction.HelloWorld1")



  def convert(value : String) : BigDecimal= {
    if(value.contains('€'))
      recalculate(toBigDecimal(value, "€"), "EUR", "PLN").getOrElse(BigDecimal(-1))
    else if (value.contains('$'))
      recalculate(toBigDecimal(value, "$"), "USD", "PLN").getOrElse(BigDecimal(-1))
    else
      recalculate(toBigDecimal(value, "zł"), "PLN", "PLN").getOrElse(BigDecimal(-1))
  }

  private def toBigDecimal(value: String, currencySeparator: String): BigDecimal = {
    logger.error("blabla1 " + value.split(currencySeparator).toList)
    logger.error("blabla2 " + currencySeparator)
    BigDecimal(value.split(currencySeparator)(0).trim.replace(",", "."))
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
