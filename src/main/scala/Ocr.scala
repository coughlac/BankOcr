import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Range.Inclusive

object Ocr {
  def scan(input: String): String = {
    //need the first 3 chars of the first 3 lines
    val lines: Array[String] = input.split('\n')
    val line1: String = lines(0).take(3)
    val line2: String = lines(1).take(3)
    val line3: String = lines(2).take(3)
    val accountNumberDigit: String = line1 + line2 + line3 match {
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
    (1 to 9).foldLeft("")((acc, _) => acc + accountNumberDigit)
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
