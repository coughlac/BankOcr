import scala.annotation.tailrec

object Ocr {
  def isValidAccountNumber(accountNumber: String) :Boolean = {
    //(d1+2*d2+3*d3 +..+9*d9) mod 11 = 0
    ((accountNumber.charAt(0).asDigit * 9 +
      accountNumber.charAt(1).asDigit * 8 +
      accountNumber.charAt(2).asDigit * 7 +
      accountNumber.charAt(3).asDigit * 6 +
      accountNumber.charAt(4).asDigit * 5 +
      accountNumber.charAt(5).asDigit * 4 +
      accountNumber.charAt(6).asDigit * 3 +
      accountNumber.charAt(7).asDigit * 2 +
      accountNumber.charAt(8).asDigit * 1 ) % 11) == 0
  }

  type PartialDigits = List[String]
  type Entry = List[String]

  def scan(input: String): List[String] = {
    val entries: List[Entry] = input.split('\n').toList.grouped(4).toList
    entries.map(accountNumber)
  }

  def accountNumber(entry: Entry): String = {
    def toPartialDigits(entryPart: String): PartialDigits = {
      entryPart.grouped(3).toList
    }

    @tailrec
    def parse(accountNumber: String, l0: PartialDigits, l1: PartialDigits, l2: PartialDigits): String = {
      if (l0.isEmpty)
        accountNumber
      else parse(accountNumber + toAccountNumberDigit(l0.head + l1.head + l2.head), l0.tail, l1.tail, l2.tail)
    }

    parse("", toPartialDigits(entry(0)), toPartialDigits(entry(1)), toPartialDigits(entry(2)))
  }

  private def toAccountNumberDigit(entryDigit: String): String = {
    entryDigit match {
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

  private val zero = "" +
    " _ " +
    "| |" +
    "|_|"

  private val one = "" +
    "   " +
    "  |" +
    "  |"

  private val two = "" +
    " _ " +
    " _|" +
    "|_ "

  private val three = "" +
    " _ " +
    " _|" +
    " _|"

  private val four = "" +
    "   " +
    "|_|" +
    "  |"

  private val five = "" +
    " _ " +
    "|_ " +
    " _|"

  private val six = "" +
    " _ " +
    "|_ " +
    "|_|"

  private val seven = "" +
    " _ " +
    "  |" +
    "  |"

  private val eight = "" +
    " _ " +
    "|_|" +
    "|_|"

  private val nine = "" +
    " _ " +
    "|_|" +
    " _|"
}
