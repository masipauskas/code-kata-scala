package com.example.bowling

import com.example.bowling._
import org.scalatest.{FlatSpec, Matchers}

class FrameSpec extends FlatSpec with Matchers {
  import Frame._

  "parse" should "parse Strike correctly" in {
    parse("X") shouldEqual(Strike(), "")
  }

  "parse" should "parse Spare correctly" in {
    parse("4/") shouldEqual(Spare(4), "")
  }

  "parse" should "parse Roll correctly" in {
    parse("9-") shouldEqual(Roll(9, 0), "")
    parse("45") shouldEqual(Roll(4, 5), "")
    parse("-9") shouldEqual(Roll(0, 9), "")
  }

  "consume" should "consume string correctly" in {
    consume("12", 1) shouldEqual "2"
    consume("12", 2) shouldEqual ""
  }

  "parseInt" should "parse integer correctly" in {
    parseInt("51", 0) shouldEqual 5
    parseInt("51", 1) shouldEqual 1
  }

  "isStrike" should "detect Strike correctly" in {
    isStrike("X") shouldBe true
    isStrike("51") shouldBe false
  }

  "isSpare" should "detect Spare correctly" in {
    isSpare("-/") shouldBe true
    isSpare("5/") shouldBe true
    isSpare("41") shouldBe false
  }
}
