package com.example.minesweeper

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by martynas on 26/01/2016.
  */
class MinesweeperSpec extends FlatSpec with Matchers {
  import Minesweeper._
  val firstBoardParsed = Board(4, 4, IndexedSeq(
    IndexedSeq(Mine, Empty, Empty, Empty),
    IndexedSeq.fill(4)(Empty),
    IndexedSeq(Empty, Mine, Empty, Empty),
    IndexedSeq.fill(4)(Empty)
  ))

  val secondBoardParsed = Board(5, 3, IndexedSeq(
    IndexedSeq(Mine, Mine, Empty, Empty, Empty),
    IndexedSeq.fill(5)(Empty),
    IndexedSeq(Empty, Mine, Empty, Empty, Empty)
  ))

  val firstBoardRevealed = Board(4 ,4, IndexedSeq(
    IndexedSeq(Mine, Nearby(1), Nearby(0), Nearby(0)),
    IndexedSeq(Nearby(2), Nearby(2), Nearby(1), Nearby(0)),
    IndexedSeq(Nearby(1), Mine, Nearby(1), Nearby(0)),
    IndexedSeq(Nearby(1), Nearby(1), Nearby(1), Nearby(0))
  ))


  val firstBoardInput = Seq("4 4", "*...", "....", ".*..", "....")
  val firstBoardSerialized = Seq("Field #1:", "*100", "2210", "1*10", "1110")
  val endOfInput = Seq("0 0")

  val secondBoardInput = Seq("3 5", "**...", ".....", ".*...")
  val secondBoardSerialized = Seq("Field #2:", "**100", "33200", "1*100")


  "play" should "reveal the empty board correctly" in {
    val board = Seq("4 4", "....", "....", "....", "....") ++ endOfInput
    val result = Seq("Field #1:", "0000", "0000", "0000", "0000")
    play(board) shouldEqual result
  }

  "play" should "reveal the testing board correctly" in {
    val board = firstBoardInput ++ endOfInput
    play(board) shouldEqual firstBoardSerialized
  }

  "play" should "reveal the testing multiple boards correctly" in {
    val boards = firstBoardInput ++ secondBoardInput ++ endOfInput
    val results = firstBoardSerialized ++ secondBoardSerialized
    play(boards) shouldEqual results
  }

  "parse" should "parse empty board correctly" in {
    val board = endOfInput
    parse(board) shouldEqual Seq.empty
  }

  "parse" should "parse single board correctly" in {
    val board = firstBoardInput ++ endOfInput
    parse(board) shouldEqual Seq(firstBoardParsed)
  }

  "parse" should "should parse multiple boards correctly" in {
    val boards = firstBoardInput ++ secondBoardInput ++ endOfInput
    val results = Seq(firstBoardParsed, secondBoardParsed)
    parse(boards) shouldEqual results
  }

  "reveal" should "reveal the board correctly" in {
    reveal(firstBoardParsed) shouldEqual firstBoardRevealed
  }

  "write" should "write the board into output string correctly" in {
    write(firstBoardRevealed, 1) shouldEqual firstBoardSerialized
  }

  "nearby" should "calculate nearby mines correctly when touches some" in {
    nearby(1, 1, firstBoardParsed) shouldEqual Nearby(2)
    nearby(3, 0, firstBoardParsed) shouldEqual Nearby(1)
  }

  "nearby" should "calculate nearby mines correctly when touches none" in {
    nearby(3, 3, firstBoardParsed) shouldEqual Nearby(0)
  }
}
