package days

case class Day24(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val moves: Map[String, (Int, Int)] = Map(
    "e" -> (0, 1),
    "se" -> (1, 0),
    "sw" -> (1, -1),
    "w" -> (0, -1),
    "nw" -> (-1, 0),
    "ne" -> (-1, 1),
  )
  val steps: List[List[String]] = list
    .map(s => {
      var res = List[String]()
      var i = 0
      while (i < s.length) {
        s(i) match {
          case 'e' | 'w' =>
            res :+= s"${s(i)}"
            i += 1
          case 's' | 'n' =>
            res :+= s"${s(i)}${s(i + 1)}"
            i += 2
        }
      }
      res
    })

  def getNextPos(step: List[String], currPos: (Int, Int) = (0, 0)): (Int, Int) =
    if (step.isEmpty) currPos
    else getNextPos(step.tail, (currPos._1 + moves(step.head)._1, currPos._2 + moves(step.head)._2))

  def getInitGrid: Map[(Int, Int), Boolean] = {
    var tiles: Map[(Int, Int), Boolean] = Map((0, 0) -> false)
    for (step <- steps) {
      val nextPos = getNextPos(step)
      tiles += (nextPos -> !tiles.getOrElse(nextPos, false))
    }
    tiles
  }

  def partOne(): Unit = {
    val tiles: Map[(Int, Int), Boolean] = getInitGrid
    printResult(1, tiles.count(_._2))
  }

  def posProcessor(grid: Map[(Int, Int), Boolean]): ((Int, Int), Boolean) => Boolean = (pos, color) => {
    val blackAdjacent = moves.values
      .count(move => grid.getOrElse((pos._1 + move._1, pos._2 + move._2), false))
    if (color && blackAdjacent != 1 && blackAdjacent != 2) !color
    else if (!color && blackAdjacent == 2) !color
    else color
  }

  def processGrid(grid: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
    var res: Map[(Int, Int), Boolean] = Map()
    val processor = posProcessor(grid)
    for ((pos, color) <- grid) {
      res += (pos -> processor(pos, color))
      for (move <- moves.values) {
        val newPos = (pos._1 + move._1, pos._2 + move._2)
        res += (newPos -> processor(newPos, res.getOrElse(newPos, false)))
      }
    }
    res
  }

  def partTwo(): Unit = {
    var tiles: Map[(Int, Int), Boolean] = getInitGrid
    for (pos <- tiles.keys; move <- moves.values) {
      val newPos = (pos._1 + move._1, pos._2 + move._2)
      tiles += (newPos -> tiles.getOrElse(newPos, false))
    }
    for (_ <- 1 to 100) {
      tiles = processGrid(tiles)
    }
    printResult(2, tiles.count(_._2))
  }
}
