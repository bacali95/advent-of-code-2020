package days

import scala.reflect.ClassTag

trait DayX {
  protected def transformDataToArray[T: ClassTag](data: String, map: String => T, separator: String = "\n"): List[T] =
    data.split(separator).map[T](map).toList

  protected def printResult(part: Int, result: Any): Unit = println(s"Part $part: $result")

  def partOne(): Unit

  def partTwo(): Unit
}
