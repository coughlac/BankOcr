

object Ocr {
  def scan(input: String): String = {
    //need the first 3 chars of the first 3 lines
    val lines = input.split('\n')

    val digit = for {
      start <- 0 to (lines(0).length-1) by 3
    } yield parseDigit(lines, start)
    digit.mkString
  }

  def parseDigit(lines: Array[String], start: Int): String = {
    val entryDigit = (0 to 2).foldLeft("")((acc, index) => acc + lines(index).substring(start, start + 3))
    toDigit(entryDigit)
  }

  def toDigit(entry: String): String = {
    entry match {
      case digit if digit == zero => "0"
      case digit if digit == one => "1"
      case digit if digit == two => "2"
      case digit if digit == three => "3"
      case digit if digit == four => "4"
      case digit if digit == five => "5"
      case digit if digit == six => "6"
      case digit if digit == seven => "7"
      case digit if digit == eight => "8"
      case digit if digit == nine => "9"
    }
  }

  val zero = "" +
    " _ " +
    "| |" +
    "|_|"

  val one = "" +
    "   " +
    "  |" +
    "  |"

  val two = "" +
    " _ " +
    " _|" +
    "|_ "

  val three = "" +
    " _ " +
    " _|" +
    " _|"

  val four = "" +
    "   " +
    "|_|" +
    "  |"

  val five = "" +
    " _ " +
    "|_ " +
    " _|"

  val six = "" +
    " _ " +
    "|_ " +
    "|_|"

  val seven = "" +
    " _ " +
    "  |" +
    "  |"

  val eight = "" +
    " _ " +
    "|_|" +
    "|_|"

  val nine = "" +
    " _ " +
    "|_|" +
    " _|"


}
