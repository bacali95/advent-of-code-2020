import days._

import scala.io.Source

object Utils {
  private def getDataForDay(day: Int): String = {
    val source = Source.fromFile(s"inputs/day$day.in")
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  def runDay(day: Int): Unit = {
    println(s"Running day $day...")
    val data = getDataForDay(day)
    val dayX = day match {
      case 1 => Day1(data)
      case 2 => Day2(data)
      case 3 => Day3(data)
      case 4 => Day4(data)
      case 11 => Day11(data)
      case 12 => Day12(data)
      case 13 => Day13(data)
      case 14 => Day14(data)
      case 15 => Day15(data)
      case 16 => Day16(data)
      case 17 => Day17(data)
      case 18 => Day18(data)
      case 19 => Day19(data)
    }
    dayX.partOne()
    dayX.partTwo()
    println()
  }
}
