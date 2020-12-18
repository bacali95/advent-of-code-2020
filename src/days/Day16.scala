package days

case class Day16(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s, "\n\n")
  val rules: Map[String, List[(Long, Long)]] = list.head
    .split("\n")
    .map(_.split(": "))
    .foldLeft(Map[String, List[(Long, Long)]]())((p, arr) =>
      p + (arr(0) -> arr(1).split(" or ")
        .map(s => {
          val arr = s.split("-").map(_.toLong)
          (arr(0), arr(1))
        })
        .toList
        )
    )
  val myTicket: List[Long] = list(1)
    .split("\n")(1)
    .split(",").map(_.toLong)
    .toList
  val nearbyTickets: List[List[Long]] = list.last
    .split("\n")
    .tail.map(_.split(",").map(_.toLong).toList)
    .toList

  def partOne(): Unit = {
    var ans = 0L
    for {
      ticket <- nearbyTickets
    } {
      ans += ticket.filter(value =>
        !rules
          .values
          .foldLeft(false)((p, rule) =>
            p || rule.foldLeft(false)((p, c) => p || (c._1 <= value && value <= c._2)))).sum
    }
    printResult(1, ans)
  }

  def partTwo(): Unit = {
    var matches: List[Set[String]] = List.fill(nearbyTickets.head.length)(Set())
    var notMatches: List[Set[String]] = List.fill(nearbyTickets.head.length)(Set())
    var rulesIndices: Map[String, Int] = Map()
    for {
      ticket <- nearbyTickets if ticket.forall(value => rules.count(rule =>
        rule._2.count(subRule => subRule._1 <= value && value <= subRule._2) > 0) > 0)
      i <- ticket.indices
    } {
      rules.foreach(rule => {
        if (rule._2.exists(sr => sr._1 <= ticket(i) && ticket(i) <= sr._2))
          matches = matches.updated(i, matches(i) + rule._1)
        else
          notMatches = notMatches.updated(i, notMatches(i) + rule._1)
      })
    }
    for (i <- matches.indices) {
      matches = matches.updated(i, matches(i).removedAll(notMatches(i)))
    }
    var done = false
    while (!done) {
      done = true
      var i = 0
      while (i < matches.length && matches(i).size != 1) i += 1
      if (i < matches.length) {
        done = false
        val head = matches(i).head
        rulesIndices = rulesIndices + (head -> i)
        for (j <- matches.indices) {
          matches = matches.updated(j, matches(j) - head)
        }
      }
    }
    printResult(2, rulesIndices
      .filter(_._1.startsWith("departure"))
      .foldLeft(1L)((p, ri) => p * myTicket(ri._2)))
  }
}
