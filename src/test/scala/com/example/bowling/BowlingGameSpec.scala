package com.example.bowling

import org.scalatest.{Matchers, FlatSpec}

class BowlingGameSpec extends FlatSpec with Matchers {
  import BowlingGame._
  "parseLine" should "parse line correctly" in {
    parseLine("1-345/234512345123X") shouldEqual Seq(
      Roll(1, 0), Roll(3, 4), Spare(5), Roll(2, 3), Roll(4, 5), Roll(1, 2), Roll(3, 4), Roll(5, 1), Roll(2, 3), Strike()
    )
  }

  "calculateScore" should "calculate simple score correctly" in {
    calculateScore(Seq(
      Roll(1, 2), Roll(3, 4), Roll(5, 1), Roll(2, 3), Roll(4, 5), Roll(1, 2), Roll(3, 4), Roll(5, 1), Roll(2, 3), Roll(4, 5)
    )) shouldBe 60
  }

  "calculateScore" should "calculate perfect game score correctly" in {
    calculateScore(Seq(
      Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike(), Strike()
    )) shouldBe 300
  }

  "calculateScore" should "calculate spare every round score correctly" in {
    calculateScore(Seq(
      Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5), Spare(5)
    )) shouldBe 150
  }

  "score" should "score a simple game correctly" in {
    score("12345123451234512345") shouldBe 60
  }

  "score" should "score a perfect game correctly" in {
    score("XXXXXXXXXXXX") shouldBe 300
  }

  "score" should "score a heartbreak game correctly" in {
    score("9-9-9-9-9-9-9-9-9-9-") shouldBe 90
  }

  "score" should "score a spare every round correctly" in {
    score("5/5/5/5/5/5/5/5/5/5/5") shouldBe 150
  }
}
