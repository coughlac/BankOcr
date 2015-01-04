
class OcrTest extends org.scalatest.FunSuite {
  test("recognises a zero") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "| || || || || || || || || |\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '0')
  }

  test("recognises a one") {
    val input = "" +
      "                           \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '1')
  }

  test("recognises a two") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '2')
  }

  test("recognises a three") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '3')
  }

  test("recognises a four") {
    val input = "" +
      "                           \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '4')
  }

  test("recognises a five") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '5')
  }

  test("recognises a six") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '6')
  }

  test("recognises a seven") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '7')
  }

  test("recognises a eight") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '8')
  }

  test("recognises a nine") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0).charAt(0) === '9')
  }

  test("recognises a nine followed by a eight") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _||_| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)
    assert(accountNumbers(0).charAt(0) === '9' && accountNumbers(0).charAt(1) === '8')
  }

  test("recognises multiple account numbers") {
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _||_| _| _| _| _| _| _| _|\n" +
      "\n" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)
    assert(accountNumbers.size === 2)
    assert(accountNumbers.exists((accountNumber) => accountNumber.startsWith("989999999")))
    assert(accountNumbers.exists((accountNumber) => accountNumber.startsWith("777777777")))
  }

  test("indicates that an account number is valid") {
    assert(Ocr.isValidAccountNumber("345882865"))
  }

  test("indicates that an account number is invalid") {
    assert(!Ocr.isValidAccountNumber("999888777"))
  }

  test("valid account number is output with no suffix") {
    val input = "" +
      " _  _  _  _  _  _  _  _    \n" +
      "| || || || || || || ||_   |\n" +
      "|_||_||_||_||_||_||_| _|  |\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)
    assert(Ocr.isValidAccountNumber("000000051"))
    assert(accountNumbers(0) === "000000051")
  }

  test("identify illegal characters in an account number and indicate that the account number is illegal") {
    val input = "" +
      "    _  _     _  _  _  _  _ \n" +
      "  | _| _||_| _ |_   ||_||_|\n" +
      "  ||_  _|  | _||_|  ||_| _ \n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0) === "1234?678? ILL")
  }

  test("identify invalid account number and indicate that the account number is invalid") {
    val input = "" +
      " _  _     _  _        _  _ \n" +
      "|_ |_ |_| _|  |  ||_||_||_ \n" +
      "|_||_|  | _|  |  |  | _| _|\n" +
      "\n"
    val accountNumbers = Ocr.scan(input)

    assert(accountNumbers(0) === "664371495 ERR")
  }
}
