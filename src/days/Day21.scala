package days

case class Day21(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)
  val ingredients: List[List[String]] = list.map(_.split(" \\(")(0).split(" ").toList)
  val allergens: Map[String, Set[String]] = list
    .foldLeft(Map[String, Set[String]]())((curr, s) => {
      val parts = s.split(" \\(contains ")
      val al = parts(1).replace(")", "").split(", ")
      var newMap = curr
      for (a <- al) {
        if (newMap.contains(a)) newMap += (a -> curr(a).intersect(parts(0).split(" ").toSet))
        else newMap += (a -> parts(0).split(" ").toSet)
      }
      newMap
    })

  def partOne(): Unit = {
    val allAllergens = allergens.values.foldLeft(Set[String]())((o, n) => o.union(n))
    printResult(1, ingredients.foldLeft(0)((p, c) => p + c.count(!allAllergens.contains(_))))
  }

  def partTwo(): Unit = {
    var search = true
    var all = allergens
    while (search) {
      search = false
      val exact = all.filter(_._2.size == 1)
      for ((_, value) <- exact) {
        val notReally = all.filter(_._2.size > 1)
        if (notReally.nonEmpty) search = true
        for ((key, value2) <- notReally) {
          all += (key -> (value2 -- value))
        }
      }
    }
    printResult(2, all.keys.toList.sorted.map(all(_).head).mkString(","))
  }
}
