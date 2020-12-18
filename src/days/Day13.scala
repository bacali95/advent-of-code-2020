package days

import scala.annotation.tailrec

case class Day13(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val startTime: Long = list.head.toLong
  val buses: List[Long] = list.tail.head
    .split(",")
    .filter(_ != "x")
    .map(_.toLong)
    .toList

  def partOne(): Unit = {
    var departTime = startTime
    var notDeparted = true
    var busId = -1L
    while (notDeparted) {
      for (id <- buses if notDeparted) {
        if (departTime % id == 0) {
          busId = id
          notDeparted = false
        }
      }
      departTime += 1
    }
    printResult(1, (departTime - startTime - 1) * busId)
  }

  def chineseRemainder(n: List[Long], a: List[Long]): Long = {
    val prod = n.product

    def mulInv(a: Long, b: Long): Long = {
      @tailrec
      def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
        if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
      }

      if (b == 1) 1
      else {
        val x1 = loop(a, b, 0, 1)
        if (x1 < 0) x1 + b else x1
      }
    }

    @tailrec
    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    (iter(n, a, 0) + prod) % prod
  }

  def partTwo(): Unit = {
    var diff = List[Long]()
    var i = 0
    var j = 0L
    for (id <- list.tail.head.split(",")) {
      if (id != "x") {
        diff :+= buses(i) - j
        i += 1
      }
      j += 1L
    }
    printResult(2, chineseRemainder(buses, diff))
  }
}
