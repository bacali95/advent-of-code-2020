package days

case class Day2(data: String) extends DayX {
  def partOne(): Unit = {
    val list = transformDataToArray[String](data, s => s)
    val answer = list.foldLeft(0) { (sum, line) =>
      val tokens = line.split(" ")
      val limits = tokens(0).split("-").map(_.toInt)
      val letter = tokens(1)(0)
      val password = tokens(2)
      val count = password.count(_ == letter)
      sum + (if (count >= limits(0) && count <= limits(1)) 1 else 0)
    }
    printResult(1, answer)
  }

  def partTwo(): Unit = {
    val list = transformDataToArray[String](data, s => s)
    val answer = list.foldLeft(0) { (sum, line) =>
      val tokens = line.split(" ")
      val pos = tokens(0).split("-").map(_.toInt)
      val letter = tokens(1)(0)
      val password = tokens(2)
      sum + (if (List(password(pos(0) - 1), password(pos(1) - 1)).count(_ == letter) == 1) 1 else 0)
    }
    printResult(2, answer)
  }
}
