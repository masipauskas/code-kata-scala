package com.example.bankOcr

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Martynas on 28/10/2015.
 */
class BankOCRSpec extends FlatSpec with Matchers {
  val ocr = new BankOCR()

  val oneToNine = Seq(
    "    _  _     _  _  _  _  _  _ ",
    "  | _| _||_||_ |_   ||_||_|| |",
    "  ||_  _|  | _||_|  ||_| _||_|"
  )
  val separator = Seq(
    "                          "
  )

  val nineToOne = Seq(
    " _  _  _  _  _  _     _  _    ",
    "| ||_||_|  ||_ |_ |_| _| _|  |",
    "|_| _||_|  ||_| _|  | _||_   |"
  )

  val illegible = Seq(
    " _  _        _  _     _  _ ",
    "|_||_   |  || |    _| _||_ ",
    "|_||_|  |  ||_| _|    _||_|"
  )

  val invalid = Seq(
    " _  _     _  _        _  _ ",
    "|_ |_ |_| _|  |  ||_||_||_ ",
    "|_||_|  | _|  |  |  | _| _|"
  )

  val valid = Seq(
    "    _  _  _  _  _  _  _  _ ",
    "|_||_   ||_ | ||_|| || || |",
    "  | _|  | _||_||_||_||_||_|"
  )



  "BankOCR" should "return an account line for each of the 4 lines" in {
    ocr.process((oneToNine ++ separator ++ nineToOne ++ separator).toIterator).length shouldBe 2
  }

  "BankOCR" should "parse a single oneToNine line correctly" in {
    ocr.process((oneToNine ++ separator).toIterator) shouldBe Seq("1234567890 ERR")
  }

  "BankOCR" should "parse two lines correctly" in {
    ocr.process((oneToNine ++ separator ++ nineToOne ++ separator).toIterator) shouldBe Seq(
      "1234567890 ERR", "0987654321 ERR"
    )
  }

  "BankOCR" should "parse an illegible account number correctly" in {
    ocr.process((illegible ++ separator).toIterator) shouldBe Seq(
      "86110??36 ILL"
    )
  }

  "BankOCR" should "parse valid account number correctly" in {
    ocr.process((valid ++ separator).toIterator) shouldBe Seq(
      "457508000"
    )
  }

  "BankOCR" should "parse invalid account number correctly" in {
    ocr.process((invalid ++ separator).toIterator) shouldBe Seq(
      "664371495 ERR"
    )
  }

  "BankOCR" should "parse a mixture of valid, illegible and invalid account numbers correctly" in {
    ocr.process((valid ++ separator ++ invalid ++ separator ++ illegible ++ separator).toIterator) shouldBe Seq(
      "457508000", "664371495 ERR", "86110??36 ILL"
    )
  }
}
