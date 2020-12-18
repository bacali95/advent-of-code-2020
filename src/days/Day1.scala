package days

case class Day1(data: String) extends DayX {
  def partOne(): Unit = {
    val list = transformDataToArray[Int](data, _.toInt)
    for (i <- 0 until list.length - 1) {
      for (j <- i + 1 until list.length) {
        if (list(i) + list(j) == 2020) {
          printResult(1, list(i) * list(j))
          return
        }
      }
    }
  }

  def partTwo(): Unit = {
    val list = transformDataToArray[Int](data, _.toInt)
    for (i <- 0 until list.length - 2) {
      for (j <- i + 1 until list.length - 1) {
        for (k <- j + 1 until list.length) {
          if (list(i) + list(j) + list(k) == 2020) {
            printResult(2, list(i) * list(j) * list(k))
            return
          }
        }
      }
    }
  }
}
