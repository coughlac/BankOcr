//User Story 1
//
//You work for a bank, which has recently purchased an ingenious machine to assist in reading letters and faxes sent in by branch offices.
// The machine scans the paper documents, and produces a file with a number of entries which each look like this:
//
//  _  _     _  _  _  _  _
//| _| _||_||_ |_   ||_||_|
//||_  _|  | _||_|  ||_| _|
//
//Each entry is 4 lines long, and each line has 27 characters.
// The first 3 lines of each entry contain an account number written using pipes and underscores, and the fourth line is blank.
// Each account number should have 9 digits, all of which should be in the range 0-9. A normal file contains around 500 entries.
//
//Your first task is to write a program that can take this file and parse it into actual account numbers.
class Acceptance extends org.scalatest.FunSuite {


  test("recognises account number of all zeros") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "| || || || || || || || || |\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.contains("000000000"))
  }

  test("recognises account number of all ones") {
    val input = "" +
      "                           \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("111111111"))
  }

  test("recognises account number of all twos") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("222222222"))
  }

  test("recognises account number of all threes") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("333333333"))
  }

  test("recognises account number of all fours") {
    val input = "" +
      "                           \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("444444444"))
  }

  test("recognises account number of all fives") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("555555555"))
  }

  test("recognises account number of all sixs") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("666666666"))
  }

  test("recognises account number of all sevens") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("777777777"))
  }

  test("recognises account number of all eights") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("888888888"))
  }

  test("recognises account number of all nines") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("999999999"))
  }

  test("recognises account number of different digits") {
    val input = "" +
      "    _  _     _  _  _  _  _ \n" +
      "  | _| _||_||_ |_   ||_||_|\n" +
      "  ||_  _|  | _||_|  ||_| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber.size === 1)
    assert(accountNumber.contains("123456789"))
  }
}
