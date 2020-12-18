package days

case class Day11(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val facts = List(
    (-1, -1), (-1, 0), (-1, 1), (0, 1),
    (1, 1), (1, 0), (1, -1), (0, -1),
  )

  def isValidPos(x: Int, y: Int): Boolean = x >= 0 && x < list.length && y >= 0 && y < list.head.length

  def partOne(): Unit = {
    var oldGrid: List[String] = list
    var newGrid: List[String] = List()
    var changed: Boolean = true
    var result: Int = 0
    while (changed) {
      changed = false
      result = 0
      for (i <- oldGrid.indices) {
        var newLine = ""
        for (j <- oldGrid(i).indices) {
          if (oldGrid(i)(j) == '.') newLine += '.'
          else {
            var occupied = 0
            for (fact <- facts if isValidPos(i + fact._1, j + fact._2)) {
              if (oldGrid(i + fact._1)(j + fact._2) == '#') occupied += 1
            }
            newLine += {
              if (oldGrid(i)(j) == 'L') if (occupied == 0) '#' else 'L'
              else if (occupied > 3) 'L' else '#'
            }
            changed ||= oldGrid(i)(j) != newLine(j)
            if (newLine(j) == '#') result += 1
          }
        }
        newGrid = newGrid :+ newLine
      }
      oldGrid = newGrid
      newGrid = List()
    }
    printResult(1, result)
  }

  def partTwo(): Unit = {
    var oldGrid: List[String] = list
    var newGrid: List[String] = List()
    var changed: Boolean = true
    var result: Int = 0
    while (changed) {
      changed = false
      result = 0
      for (i <- oldGrid.indices) {
        var newLine = ""
        for (j <- oldGrid(i).indices) {
          if (oldGrid(i)(j) == '.') newLine += '.'
          else {
            var occupied = 0;
            for (fact <- facts) {
              var x = i + fact._1
              var y = j + fact._2
              while (isValidPos(x, y) && oldGrid(x)(y) == '.') {
                x = x + fact._1
                y = y + fact._2
              }
              if (isValidPos(x, y) && oldGrid(x)(y) == '#') occupied += 1
            }
            newLine += {
              if (oldGrid(i)(j) == 'L') if (occupied == 0) '#' else 'L'
              else if (occupied > 4) 'L' else '#'
            }
            changed ||= oldGrid(i)(j) != newLine(j)
            if (newLine(j) == '#') result += 1
          }
        }
        newGrid = newGrid :+ newLine
      }
      oldGrid = newGrid
      newGrid = List()
    }
    printResult(2, result)
  }
}
