package com.example.montyHall

import org.scalatest.{Matchers, FlatSpec}

class MontyHallSpec extends FlatSpec with Matchers {

  import MontyHall._

  case class R(var v: Seq[Int]) {
    def random(n: Int) = {
      val value = v.head
      v = v.tail
      value
    }
  }

  "play variable change strategy" should "always pick the 2nd door" in {
    val pickSequenceWin = R(Seq(0, 1, 0, 0))
    playGameVariableChange(IndexedSeq(Lose, Lose, Win), pickSequenceWin.random) shouldEqual Win

    val pickSequenceLose = R(Seq(0, 1, 0))
    playGameVariableChange(IndexedSeq(Win, Lose, Lose), pickSequenceLose.random) shouldEqual Lose
  }

  "play first choice strategy" should "always pick the 1st door" in {
    val pickSequenceWin = R(Seq(0, 1, 0, 0))
    playGameFirstChoice(IndexedSeq(Lose, Lose, Win), pickSequenceWin.random) shouldEqual Lose

    val pickSequenceLose = R(Seq(0, 1, 0))
    playGameFirstChoice(IndexedSeq(Win, Lose, Lose), pickSequenceLose.random) shouldEqual Win
  }


  "pick door" should "always pick the door which is determined by random" in {
    val doors = IndexedSeq(Lose, Win, Lose)
    val rLose = (i: Int) => 0
    val rWin = (i: Int) => 1

    val (doorLose, remainingWin) = pickDoor(doors)(rLose)
    doorLose shouldEqual Lose
    remainingWin shouldEqual IndexedSeq(Win, Lose)

    val (doorWin, remainingLose) = pickDoor(doors)(rWin)
    doorWin shouldEqual Win
    remainingLose shouldEqual IndexedSeq(Lose, Lose)
  }


  "generate door" should "always generate the door with exactly one win" in {
    for (i <- 1 to 100) {
      generateDoor(3)(random).collect{ case Win => true }.length shouldEqual 1
    }
  }

  "random" should "always generate value within the range" in {
    for (i <- 1 to 10) {
      val result = random(10)
      (result >= 0 && result <= 10) shouldBe true
    }
  }

  "play" should "generate winning statistics" in {
    val fixedFirstChoiceWin = (1 to 10000)
      .map{ _ => play(3)(random)(playGameFirstChoice) }
      .collect { case Win => true }.length / 100
    val variableChoiceWin = (1 to 10000)
      .map{ _ => play(3)(random)(playGameVariableChange) }
      .collect { case Win => true }.length / 100

    println(s"Fixed choice: $fixedFirstChoiceWin, Variable choice: $variableChoiceWin")
  }
}
