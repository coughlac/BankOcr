import scala.annotation.tailrec

object OcrApp  extends App {
  override def main(args: Array[String]) {
    println((100000000 to 199999999).find(i => {
      val s: String = Integer.toString(i)
      println(s)
      val number: String = Ocr.correctAccountNumber(s)
      println(number)
      number.endsWith("ERR")
    }))
  }
}

object Ocr {

  val alternativeDigits = Map('1' -> Set('7'), '2' -> Set.empty, '3'-> Set('9'),
    '4' -> Set.empty, '5' -> Set('6', '9'), '6' -> Set('5', '8'),
    '7' -> Set('1'), '8' -> Set('0', '6', '9'), '9' -> Set('3', '5', '8'), '0' -> Set('8')
  )

  def correctAccountNumber(accountNumber: String): String = {
    def getAlternativeAccountNumbers = {
      for {
        index <- 0 to 8
        alternative <- alternativeDigits(accountNumber.charAt(index))
      } yield accountNumber.updated(index, alternative)
    }
    if(isValidAccountNumber(accountNumber))
      accountNumber
    else if (accountNumber.contains("?"))
      accountNumber + " ILL"
    else {
      getAlternativeAccountNumbers.filter(isValidAccountNumber) match {
        case validAccountNumbers if validAccountNumbers.size > 1 => accountNumber + " AMB"
        case validAccountNumbers if validAccountNumbers.size == 1 => validAccountNumbers.head
        case _ => accountNumber + " ERR"
      }
    }
  }

  def isValidAccountNumber(accountNumber: String): Boolean = {
    val sumOfProduct: Int = (1 to 9).foldLeft(0)((acc, index) => acc + (accountNumber.charAt(9 - index).asDigit * index))
    sumOfProduct % 11 == 0
  }

  type PartialDigits = List[String]
  type Entry = List[String]

  def scan(input: String): List[String] = {
    val entries: List[Entry] = input.split('\n').toList.grouped(4).toList
    entries.map(entry => correctAccountNumber(accountNumber(entry)))
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
      case _ => "?"
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
