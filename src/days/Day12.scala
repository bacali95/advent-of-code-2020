package days

case class Day12(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)

  def partOne(): Unit = {
    var north = 0
    var east = 0
    var dir = 0
    for (move <- list) {
      val n = move.substring(1).toInt
      move(0) match {
        case 'N' => north += n
        case 'S' => north -= n
        case 'E' => east += n
        case 'W' => east -= n
        case 'L' =>
          dir -= (n % 360) / 90
          dir += 4
          dir %= 4
        case 'R' =>
          dir += (n % 360) / 90
          dir %= 4
        case 'F' => dir match {
          case 0 => east += n
          case 1 => north -= n
          case 2 => east -= n
          case 3 => north += n
        }
      }
    }
    printResult(1, Math.abs(north) + Math.abs(east))
  }

  def partTwo(): Unit = {
    var north = 0
    var east = 0
    var northS = 1
    var eastS = 10
    for (move <- list) {
      val n = move.substring(1).toInt
      move(0) match {
        case 'N' => northS += n
        case 'S' => northS -= n
        case 'E' => eastS += n
        case 'W' => eastS -= n
        case 'R' =>
          val dir = (n % 360) / 90
          var aux = 0
          dir match {
            case 1 =>
              aux = eastS
              eastS = northS
              northS = -aux
            case 2 =>
              eastS = -eastS
              northS = -northS
            case 3 =>
              aux = eastS
              eastS = -northS
              northS = aux
            case _ =>
          }
        case 'L' =>
          val dir = (n % 360) / 90
          var aux = 0
          dir match {
            case 1 =>
              aux = eastS
              eastS = -northS
              northS = aux
            case 2 =>
              eastS = -eastS
              northS = -northS
            case 3 =>
              aux = eastS
              eastS = northS
              northS = -aux
            case _ =>
          }
        case 'F' =>
          east += n * eastS
          north += n * northS
      }
    }
    printResult(1, Math.abs(north) + Math.abs(east))
  }
}
