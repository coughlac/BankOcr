import scala.collection.immutable.IndexedSeq

object Ocr {
  def scan(input: String): String = {
    //need the first 3 chars of the first 3 lines
    val lines = input.split('\n')

    val digit: IndexedSeq[String] = for {
      n <- 0 to 8
      start <- 0 to 26 by 3
    } yield parseDigit(lines, start)
    digit.mkString
  }

  def parseDigit(lines: Array[String], start: Int): String = {
    // val entryDigit: String = (start to start + 2).foldLeft("")((acc, index) => acc + lines(index).take(3))
    val line1 = lines(0).substring(start, start + 3)
    val line2 = lines(1).substring(start, start + 3)
    val line3 = lines(2).substring(start, start + 3)
    val entryDigit = line1 + line2 + line3
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
