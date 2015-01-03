
class OcrTest extends org.scalatest.FunSuite {
  test("recognises a zero"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "| || || || || || || || || |\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '0')
  }

  test("recognises a one"){
    val input = "" +
      "                           \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '1')
  }

  test("recognises a two"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '2')
  }

  test("recognises a three"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '3')
  }

  test("recognises a four"){
    val input = "" +
      "                           \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '4')
  }

  test("recognises a five"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '5')
  }

  test("recognises a six"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '6')
  }

  test("recognises a seven"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '7')
  }

  test("recognises a eight"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '8')
  }

  test("recognises a nine"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _| _| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)

    assert(accountNumber(0).charAt(0) === '9')
  }

  test("recognises a nine followed by a eight"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _||_| _| _| _| _| _| _| _|\n" +
      "\n"
    val accountNumber = Ocr.scan(input)
    assert(accountNumber(0).charAt(0) === '9' && accountNumber(0).charAt(1) === '8')
  }

  test("recognises multiple account numbers"){
    val input = "" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      " _||_| _| _| _| _| _| _| _|\n" +
      "\n" +
      " _  _  _  _  _  _  _  _  _ \n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "  |  |  |  |  |  |  |  |  |\n" +
      "\n"
    val accountNumber = Ocr.scan(input)
    assert(accountNumber.size === 2)
    assert(accountNumber.contains("989999999"))
    assert(accountNumber.contains("777777777"))
  }
}
