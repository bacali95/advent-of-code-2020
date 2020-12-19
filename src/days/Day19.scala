package days

case class Day19(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s, "\n\n")
  val strings: Array[String] = list.head.split("\n")
  var rules: Map[Int, String] = strings
    .foldLeft(Map[Int, String]())((arr, rule) => {
      val a = rule.split(": ")
      arr + (a(0).toInt -> a(1))
    })
  var messages: List[String] = list.last.split("\n").toList

  def buildRule(rules: Map[Int, String], index: Int = 0, depth: Int = 0): String = {
    if (depth >= 40) return ""
    val rule = rules(index)
    if (rule.startsWith("\"")) rule.replaceAll("\"", "")
    else {
      val or = rule.split(" \\| ")
      if (or.length == 1) rule.split(" ").map(i => buildRule(rules, i.toInt, depth + 1)).mkString
      else or.map(_.split(" ").map(i => buildRule(rules, i.toInt, depth + 1)).mkString).mkString("(", "|", ")")
    }
  }

  def partOne(): Unit = {
    val rule = buildRule(rules)
    printResult(1, messages.count(_.matches(rule)))
  }

  def partTwo(): Unit = {
    val newRules = rules + (8 -> "42 | 42 8") + (11 -> "42 31 | 42 11 31")
    val rule = buildRule(newRules)
    printResult(2, messages.count(_.matches(rule)))
  }
}
