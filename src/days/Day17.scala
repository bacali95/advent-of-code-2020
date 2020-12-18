package days

case class Day17(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)

  def isActive3D(list: List[List[List[Char]]], i: Int, j: Int, k: Int): Boolean = {
    val x = list.length
    val y = list.head.length
    val z = list.head.head.length
    if (i < 0 || i >= x || j < 0 || j >= y || k < 0 || k >= z) false
    else list(i)(j)(k) == '#'
  }

  def iterate3D(list: List[List[List[Char]]]): List[List[List[Char]]] = {
    val x = list.length
    val y = list.head.length
    val z = list.head.head.length
    var res = List.fill(x + 2)(List.fill(y + 2)(List.fill(z + 2)('.')))
    for (i <- 0 until x + 2) {
      for (j <- 0 until y + 2) {
        for (k <- 0 until z + 2) {
          var count = 0
          for (is <- -1 to 1) {
            for (js <- -1 to 1) {
              for (ks <- -1 to 1) {
                if (is != 0 || js != 0 || ks != 0)
                  if (isActive3D(list, i - 1 + is, j - 1 + js, k - 1 + ks))
                    count += 1
              }
            }
          }
          if (isActive3D(list, i - 1, j - 1, k - 1)) {
            res = res.updated(i, res(i).updated(j, res(i)(j).updated(k, if (count == 2 || count == 3) '#' else '.')))
          } else {
            res = res.updated(i, res(i).updated(j, res(i)(j).updated(k, if (count == 3) '#' else '.')))
          }
        }
      }
    }
    res
  }

  def partOne(): Unit = {
    var input: List[List[List[Char]]] = List(list.map(_.toCharArray.toList))
    for (_ <- 1 to 6)
      input = iterate3D(input)
    printResult(1, input.foldLeft(0)((p, c) => p + c.foldLeft(0)((pp, cc) => pp + cc.count(_ == '#'))))
  }

  def isActive4D(list: List[List[List[List[Char]]]], i: Int, j: Int, k: Int, l: Int): Boolean = {
    val x = list.length
    val y = list.head.length
    val z = list.head.head.length
    val w = list.head.head.head.length
    if (i < 0 || i >= x || j < 0 || j >= y || k < 0 || k >= z || l < 0 || l >= w) false
    else list(i)(j)(k)(l) == '#'
  }

  def iterate4D(list: List[List[List[List[Char]]]]): List[List[List[List[Char]]]] = {
    val x = list.length
    val y = list.head.length
    val z = list.head.head.length
    val w = list.head.head.head.length
    var res = List.fill(x + 2)(List.fill(y + 2)(List.fill(z + 2)(List.fill(w + 2)('.'))))
    for (i <- 0 until x + 2) {
      for (j <- 0 until y + 2) {
        for (k <- 0 until z + 2) {
          for (l <- 0 until w + 2) {
            var count = 0
            for (is <- -1 to 1) {
              for (js <- -1 to 1) {
                for (ks <- -1 to 1) {
                  for (ls <- -1 to 1) {
                    if (is != 0 || js != 0 || ks != 0 || ls != 0)
                      if (isActive4D(list, i - 1 + is, j - 1 + js, k - 1 + ks, l - 1 + ls))
                        count += 1
                  }
                }
              }
            }
            if (isActive4D(list, i - 1, j - 1, k - 1, l - 1)) {
              res = res.updated(i, res(i).updated(j, res(i)(j).updated(k, res(i)(j)(k).updated(l, if (count == 2 || count == 3) '#' else '.'))))
            } else {
              res = res.updated(i, res(i).updated(j, res(i)(j).updated(k, res(i)(j)(k).updated(l, if (count == 3) '#' else '.'))))
            }
          }
        }
      }
    }
    res
  }

  def partTwo(): Unit = {
    var input: List[List[List[List[Char]]]] = List(List(list.map(_.toCharArray.toList)))
    for (_ <- 1 to 6)
      input = iterate4D(input)
    printResult(2, input.foldLeft(0)((p, c) => p + c.foldLeft(0)((pp, cc) => pp + cc.foldLeft(0)((ppp, ccc) => ppp + ccc.count(_ == '#')))))
  }
}
