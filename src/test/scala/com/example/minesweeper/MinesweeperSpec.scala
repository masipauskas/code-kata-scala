package com.example.minesweeper

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by martynas on 26/01/2016.
  */
class MinesweeperSpec extends FlatSpec with Matchers {
  import Minesweeper._
  "play" should "reveal the empty board correctly" in {
    val board = Seq(
      "4 4", "....", "....", "....", "....", "0 0"
    )
    val result = Seq(
      "Field #1:", "0000", "0000", "0000", "0000"
    )
    play(board) shouldEqual result
  }

  "play" should "reveal the testing board correctly" in {
    val board = Seq(
     "4 4", "*...", "....", ".*..", "....", "0 0"
    )
    val result = Seq(
      "Field #1:", "*100", "2210", "1*10", "1110"
    )
    play(board) shouldEqual result
  }

  "play" should "reveal the testing multiple boards correctly" in {
    val boards = Seq(
      "4 4", "*...", "....", ".*..", "....",
      "3 5", "**...", ".....", ".*...",
      "0 0"
    )
    val results = Seq(

      "Field #1:", "*100", "2210", "1*10", "1110",
      "Field #2:", "**100", "33200", "1*100"
    )
    play(boards) shouldEqual results
  }

  "parse" should "parse empty board correctly" in {
    val board = Seq(
      "0 0"
    )

    val result = Seq()
    parse(board) shouldEqual result
  }

  "parse" should "parse single board correctly" in {
    val board = Seq(
      "4 4", "*...", "....", ".*..", "....", "0 0"
    )

    val result = Seq(Board(4 ,4, IndexedSeq(
      IndexedSeq(Mine, Empty, Empty, Empty),
      IndexedSeq.fill(4)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty),
      IndexedSeq.fill(4)(Empty)
    )))

    parse(board) shouldEqual result
  }

  "parse" should "should parse multiple boards correctly" in {
    val boards = Seq(
      "4 4", "*...", "....", ".*..", "....",
      "3 5", "**...", ".....", ".*...",
      "0 0"
    )
    val results = Seq(Board(4, 4, IndexedSeq(
      IndexedSeq(Mine, Empty, Empty, Empty),
      IndexedSeq.fill(4)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty),
      IndexedSeq.fill(4)(Empty)
    )), Board(5, 3, IndexedSeq(
      IndexedSeq(Mine, Mine, Empty, Empty, Empty),
      IndexedSeq.fill(5)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty, Empty)
    )))

    parse(boards) shouldEqual results
  }

  "reveal" should "reveal the board correctly" in {
    val board = Board(4, 4, IndexedSeq(
      IndexedSeq(Mine, Empty, Empty, Empty),
      IndexedSeq.fill(4)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty),
      IndexedSeq.fill(4)(Empty)
    ))

    val result = Board(4 ,4, IndexedSeq(
      IndexedSeq(Mine, Nearby(1), Nearby(0), Nearby(0)),
      IndexedSeq(Nearby(2), Nearby(2), Nearby(1), Nearby(0)),
      IndexedSeq(Nearby(1), Mine, Nearby(1), Nearby(0)),
      IndexedSeq(Nearby(1), Nearby(1), Nearby(1), Nearby(0))
    ))

    reveal(board) shouldEqual result
  }

  "write" should "write the board into output string correctly" in {
    val board = Board(4 ,4, IndexedSeq(
      IndexedSeq(Mine, Nearby(1), Nearby(0), Nearby(0)),
      IndexedSeq(Nearby(2), Nearby(2), Nearby(1), Nearby(0)),
      IndexedSeq(Nearby(1), Mine, Nearby(1), Nearby(0)),
      IndexedSeq(Nearby(1), Nearby(1), Nearby(1), Nearby(0))
    ))

    val result = Seq("Field #1:", "*100", "2210", "1*10", "1110")

    write(board, 1) shouldEqual result
  }

  "nearby" should "calculate nearby mines correctly when touches some" in {
    val board = Board(4, 4, IndexedSeq(
      IndexedSeq(Mine, Empty, Empty, Empty),
      IndexedSeq.fill(4)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty),
      IndexedSeq.fill(4)(Empty)
    ))

    nearby(1, 1, board) shouldEqual Nearby(2)
    nearby(3, 0, board) shouldEqual Nearby(1)
  }

  "nearby" should "calculate nearby mines correctly when touches none" in {
    val board = Board(4, 4, IndexedSeq(
      IndexedSeq(Mine, Empty, Empty, Empty),
      IndexedSeq.fill(4)(Empty),
      IndexedSeq(Empty, Mine, Empty, Empty),
      IndexedSeq.fill(4)(Empty)
    ))

    nearby(3, 3, board) shouldEqual Nearby(0)
  }
}
