package days

case class Day3(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)

  def walk(stepX: Int, stepY: Int): (Int, Int) => Long = (x, y) =>
    if (x >= list.size) 0
    else (if (list(x)(y) == '#') 1 else 0) + walk(stepX, stepY)(x + stepX, (y + stepY) % list.head.length)

  def partOne(): Unit = {
    printResult(1, walk(1, 3)(0, 0))
  }

  def partTwo(): Unit = {
    printResult(2,
      walk(1, 1)(0, 0) *
        walk(1, 3)(0, 0) *
        walk(1, 5)(0, 0) *
        walk(1, 7)(0, 0) *
        walk(2, 1)(0, 0)
    )
  }
}
