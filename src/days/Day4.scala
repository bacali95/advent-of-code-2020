package days

case class Day4(data: String) extends DayX {
  val list: List[String] = transformDataToArray[String](data, s => s, "\n\n")

  def partOne(): Unit = {
    printResult(1,
      list
        .map(_.split("[ \n]"))
        .map(_.map(_.split(":")(0)))
        .count(f => f.length == 8 || (f.length == 7 && !f.contains("cid")))
    )
  }

  def partTwo(): Unit = {
    def passportIsValid(passport: Map[String, String]): Boolean = {
      var valid = true
      val byr = passport.getOrElse("byr", "0").toInt
      valid &= byr > 1919 && byr < 2003
      val iyr = passport.getOrElse("iyr", "0").toInt
      valid &= iyr > 2009 && iyr < 2021
      val eyr = passport.getOrElse("eyr", "0").toInt
      valid &= eyr > 2019 && eyr < 2031
      val hgt = passport.getOrElse("hgt", "-")
      valid &= (
        hgt.endsWith("cm") &&
          hgt.replace("cm", "").toInt > 149 &&
          hgt.replace("cm", "").toInt < 194
        ) || (
        hgt.endsWith("in") &&
          hgt.replace("in", "").toInt > 58 &&
          hgt.replace("in", "").toInt < 77
        )
      val hcl = passport.getOrElse("hcl", "")
      valid &= hcl.matches("#[0-9a-f]{6}")
      val ecl = passport.getOrElse("ecl", "")
      valid &= ecl.matches("(amb|blu|brn|gry|grn|hzl|oth)")
      val pid = passport.getOrElse("pid", "")
      valid &= pid.matches("[0-9]{9}")
      valid
    }

    printResult(2,
      list
        .map(_.split("[ \n]"))
        .map(_.map(s => (s.split(":")(0), s.split(":")(1))).toMap)
        .count(passportIsValid)
    )
  }
}
