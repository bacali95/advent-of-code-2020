package days

import scala.math.{log, pow}

case class Day14(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s)

  def partOne(): Unit = {
    var mask: String = ""
    var addr = Map[Long, Long]()
    for (line <- list) {
      if (line.startsWith("mask")) mask = line.substring(7).reverse
      else {
        val op = line.replace("mem[", "").split("] = ").map(_.toLong)
        val bs = op(1).toBinaryString.reverse
        val c = bs + ("0" * (mask.length - bs.length))
        var fact: Long = 0L
        var _ans: Long = 0L
        for (b <- mask) {
          _ans += (if (b == '1' || (b == 'X' && c(fact.toInt) == '1')) pow(2, fact).toLong else 0L)
          fact += 1L
        }
        addr += (op(0) -> _ans)
      }
    }
    printResult(1, addr.values.sum)
  }

  def binaryToLong(binary: String): Long = {
    def binaryToLongHelper(binary: String, index: Int = 0): Long =
      if (index == binary.length) 0
      else (if (binary(index) == '1') pow(2, index).toLong else 0) + binaryToLongHelper(binary, index + 1)

    binaryToLongHelper(binary)
  }

  def partTwo(): Unit = {
    var mask: String = ""
    var addr = Map[Long, Long]()

    def applyMask(address: String, value: Long, index: Int): Unit = {
      if (index == mask.length) addr += (binaryToLong(address) -> value)
      else if (mask(index) != 'X')
        applyMask(address.updated(index, if (mask(index) == '1' || address(index) == '1') '1' else '0'), value, index + 1)
      else {
        applyMask(address.updated(index, '0'), value, index + 1)
        applyMask(address.updated(index, '1'), value, index + 1)
      }
    }

    for (line <- list) {
      if (line.startsWith("mask")) mask = line.substring(7).reverse
      else {
        val op = line.replace("mem[", "").split("] = ").map(_.toLong)
        val bs = op(0).toBinaryString.reverse
        applyMask(bs + ("0" * (mask.length - bs.length)), op(1), 0)
      }
    }
    printResult(2, addr.values.sum)
  }
}
