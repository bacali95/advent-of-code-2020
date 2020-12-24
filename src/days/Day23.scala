package days

case class Day23(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val initCircle: Vector[Long] = list.head
    .split("")
    .map(_.toLong)
    .toVector
  val initChain: Vector[Long] = {
    var res: Vector[Long] = Vector.fill(10)(0)
    for (i <- initCircle.indices) {
      res = if (i < initCircle.length - 1) res.updated(initCircle(i).toInt, initCircle(i + 1))
      else res.updated(initCircle(i).toInt, initCircle.head)
    }
    res
  }

  def process(chain: Vector[Long], cup: Long, max: Long = 9): Vector[Long] = {
    var res: Vector[Long] = chain
    var nextCup: Long = cup
    var cups: List[Long] = List(res(nextCup.toInt))
    for (_ <- 1 to 2) {
      cups :+= res(cups.last.toInt)
    }
    res = res.updated(nextCup.toInt, res(cups.last.toInt))
    nextCup -= 1
    while (nextCup < 1 || cups.contains(nextCup)) {
      if (nextCup < 1) nextCup = max
      else nextCup -= 1
    }
    val temp = res(nextCup.toInt)
    res = res.updated(nextCup.toInt, cups.head)
    res = res.updated(cups.last.toInt, temp)
    res
  }

  def getStringCircle(chain: Vector[Long]): String = {
    var res = ""
    var cup = 1
    while (chain(cup) != 1) {
      res += chain(cup)
      cup = chain(cup).toInt
    }
    res
  }

  def partOne(): Unit = {
    var chain = initChain
    var cup = initCircle.head
    for (_ <- 1 to 100) {
      chain = process(chain, cup)
      cup = chain(cup.toInt)
    }
    printResult(1, getStringCircle(chain).toLong)
  }

  def partTwo(): Unit = {
    var newCircle: Vector[Long] = initCircle
    for (i <- 10L to 1000000L)
      newCircle :+= i
    var chain = {
      var res: Vector[Long] = Vector.fill(1000001)(0)
      for (i <- newCircle.indices) {
        res = if (i < newCircle.length - 1) res.updated(newCircle(i).toInt, newCircle(i + 1))
        else res.updated(newCircle(i).toInt, newCircle.head)
      }
      res
    }
    var cup = initCircle.head
    for (i <- 1 to 10000000) {
      chain = process(chain, cup, 1000000)
      cup = chain(cup.toInt)
    }
    printResult(2, chain(1) * chain(chain(1).toInt))
  }
}
