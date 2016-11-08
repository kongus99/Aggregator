package services

object ThresholdLevenshtein {
  def count(_s: String, _t: String, threshold: Int): Int = {
    val (s, t) = if (_s.length > _t.length) (_s, _t) else (_t, _s)
    val slen = s.length
    val tlen = t.length

    var prev = Array.fill[Int](tlen+1)(Int.MaxValue)
    var curr = Array.fill[Int](tlen+1)(Int.MaxValue)
    for (n <- 0 until math.min(tlen+1, threshold+1)) prev(n) = n

    for (row <- 1 until (slen+1)) {
      curr(0) = row
      val min = math.min(tlen+1, math.max(1, row - threshold))
      val max = math.min(tlen+1, row + threshold + 1)
      // println("row[" + row + "] min=" + min + " max=" + max)

      if (min > 1) curr(min-1) = Int.MaxValue
      for (col <- min until max) {
        curr(col) = if (s(row-1) == t(col-1)) prev(col-1)
        else math.min(prev(col-1), math.min(curr(col-1), prev(col))) + 1
      }
      // println("row[" + row + "]: " + curr.mkString(" "))
      prev = curr
      curr = Array.fill[Int](tlen+1)(Int.MaxValue)
    }

    prev(tlen)
  }
}


