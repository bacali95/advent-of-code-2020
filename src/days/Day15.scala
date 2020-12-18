package days

case class Day15(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val initNumbers: List[Int] = list.head.split(",").map(_.toInt).toList

  def getResult(limit: Int): Int = {
    val historic: Array[(Int, Int)] = Array.fill(limit + 1)(null)
    for (i <- initNumbers.indices) historic(initNumbers(i)) = (i, -1)

    def addNumber(toAdd: Int, turn: Int): Unit =
      if (historic(toAdd) == null) historic(toAdd) = (turn, -1)
      else historic(toAdd) = (turn, historic(toAdd)._1)

    var turn = initNumbers.length
    var last = initNumbers.last
    while (turn < limit) {
      if (historic(last)._2 == -1) {
        last = 0
        addNumber(0, turn)
      } else {
        val toAdd = historic(last)._1 - historic(last)._2
        last = toAdd
        addNumber(toAdd, turn)
      }
      turn += 1
    }
    last
  }

  def partOne(): Unit = {
    printResult(1, getResult(2020))
  }

  def partTwo(): Unit = {
    printResult(2, getResult(30000000))
  }
}
