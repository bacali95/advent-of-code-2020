package days

case class Day18(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)


  def partOne(): Unit = {
    var expr: List[String] = List()
    var index: Int = 0

    def applyOp(a: Long, op: Char, b: Long): Long = if (op == '+') a + b else a * b

    def evaluate(): Long = {
      var res = 0L
      var lastOp = '+'
      while (index < expr.length) {
        expr(index) match {
          case "(" =>
            index += 1
            res = applyOp(res, lastOp, evaluate())
          case ")" =>
            index += 1
            return res
          case "+" =>
            index += 1
            lastOp = '+'
          case "*" =>
            index += 1
            lastOp = '*'
          case _ =>
            res = applyOp(res, lastOp, expr(index).toLong)
            index += 1
        }
      }
      res
    }

    var res = 0L
    for (exp <- list) {
      expr = exp.split(" ").toList.flatMap(s =>
        if (s.startsWith("(")) List.concat(s.substring(0, s.lastIndexOf("(") + 1).split(""), List(s.substring(s.lastIndexOf("(") + 1)))
        else if (s.endsWith(")")) List.concat(List(s.substring(0, s.indexOf(")"))), s.substring(s.indexOf(")")).split(""))
        else List(s)
      )
      index = 0
      res += evaluate()
    }
    printResult(1, res)
  }

  def partTwo(): Unit = {
    var expr: List[String] = List()
    var index: Int = 0

    def evaluate(): Long = {
      var toMultiply: List[Long] = List()
      var res = 0L
      while (index < expr.length) {
        expr(index) match {
          case "(" =>
            index += 1
            res += evaluate()
          case ")" =>
            index += 1
            toMultiply :+= res
            return toMultiply.product
          case "+" =>
            index += 1
          case "*" =>
            index += 1
            toMultiply :+= res
            res = 0L
          case _ =>
            res += expr(index).toLong
            index += 1
        }
      }
      toMultiply :+= res
      toMultiply.product
    }

    var res = 0L
    for (exp <- list) {
      expr = exp.split(" ").toList.flatMap(s =>
        if (s.startsWith("(")) List.concat(s.substring(0, s.lastIndexOf("(") + 1).split(""), List(s.substring(s.lastIndexOf("(") + 1)))
        else if (s.endsWith(")")) List.concat(List(s.substring(0, s.indexOf(")"))), s.substring(s.indexOf(")")).split(""))
        else List(s)
      )
      index = 0
      res += evaluate()
    }
    printResult(2, res)
  }
}
