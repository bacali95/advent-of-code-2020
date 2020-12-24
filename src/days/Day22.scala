package days

case class Day22(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s, "\n\n")
  val player1: List[Int] = list.head.split("\n").tail.map(_.toInt).toList
  val player2: List[Int] = list.last.split("\n").tail.map(_.toInt).toList

  def getScore(list: List[Int], acc: Int = 0): Int =
    if (list.isEmpty) acc
    else getScore(list.tail, acc + list.size * list.head)

  def partOne(): Unit = {
    var player1Aux = player1
    var player2Aux = player2
    while (player1Aux.nonEmpty && player2Aux.nonEmpty) {
      val p1 = player1Aux.head
      player1Aux = player1Aux.tail
      val p2 = player2Aux.head
      player2Aux = player2Aux.tail
      if (p1 > p2) {
        player1Aux :+= p1
        player1Aux :+= p2
      } else {
        player2Aux :+= p2
        player2Aux :+= p1
      }
    }
    printResult(1, getScore(if (player1Aux.nonEmpty) player1Aux else player2Aux))
  }


  def playGame(player1: List[Int], player2: List[Int]): (Int, Int) = {
    var player1Aux = player1
    var player2Aux = player2
    var playedRounds: Set[(Int, Int)] = Set()
    while (player1Aux.nonEmpty && player2Aux.nonEmpty) {
      val roundId = (getScore(player1Aux), getScore(player2Aux))
      if (playedRounds.contains(roundId)) return (1, roundId._1)
      playedRounds += roundId
      val p1 = player1Aux.head
      player1Aux = player1Aux.tail
      val p2 = player2Aux.head
      player2Aux = player2Aux.tail
      var winner = if (p1 > p2) 1 else 2
      if (player1Aux.length >= p1 && player2Aux.length >= p2) {
        winner = playGame(player1Aux.take(p1), player2Aux.take(p2))._1
      }
      if (winner == 1) {
        player1Aux :+= p1
        player1Aux :+= p2
      } else {
        player2Aux :+= p2
        player2Aux :+= p1
      }
    }
    if (player1Aux.nonEmpty) (1, getScore(player1Aux))
    else (2, getScore(player2Aux))
  }

  def partTwo(): Unit = {
    printResult(2, playGame(player1, player2)._2)
  }
}
