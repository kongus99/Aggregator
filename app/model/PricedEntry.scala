package model


class PricedEntry(val price: Option[Float], val discounted: Option[Float]) {
  val percentage: Option[Float] = for {
    d <- discounted
    p <- price
  } yield math.round(((p - d) / p) * 100)
}
