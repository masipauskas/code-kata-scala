package com.example.minesweeper

/**
  * Created by martynas on 26/01/2016.
  */
object Minesweeper {
  def play(board: Seq[String]): Seq[String] = {
      parse(board)
        .map(reveal)
        .zipWithIndex
        .flatMap { case (b, index) => write(b, index + 1) }
  }

  def parse(lines: Seq[String]): Seq[Board] = {
    def charToCell(c: Char): Cell = c match {
      case '*' => Mine
      case '.' => Empty
    }

    def parseBoard(lines: Seq[String]): (Board, Seq[String]) = {
      val params = lines.head.split(" ").map(_.toInt).take(2)
      val (height, width) = (params.head, params.last)
      val (board, remaining) = lines.tail.splitAt(height)
      (Board(width, height, board.map(_.map(charToCell)).toIndexedSeq), remaining)
    }

    def isEndOfInput(line: String) = line == "0 0"

    if (isEndOfInput(lines.head)) Seq.empty
    else {
      var remaining = lines
      var parsed: Seq[Board] = Seq.empty
      do {
        val (board, linesRemaining) = parseBoard(remaining)
        remaining = linesRemaining
        parsed = parsed :+ board
      } while (!isEndOfInput(remaining.head))

      parsed
    }
  }

  def reveal(board: Board): Board = {
    board.copy(cells = board.cells.zipWithIndex.map {
      case (row, idX) => row.zipWithIndex.map { case (cell, idY) => nearby(idX, idY, board)}
    })
  }

  def nearby(x: Int, y: Int, board: Board): Cell = {
    def hasMine(x: Int, y: Int) = {
      if (x < 0 || y < 0 || x >= board.height || y >= board.width) 0
      else if (board.cells(x)(y) == Mine) 1 else 0
    }

    val isMine = board.cells(x)(y) == Mine

    val square = Seq((x + 1, y + 1), (x, y + 1), (x - 1, y + 1),
      (x + 1, y), (x - 1, y),
      (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
    )

    if (isMine) Mine
    else {
      val nearbyMines = square.map { case (x, y) => hasMine(x, y)}.sum
      Nearby(nearbyMines)
    }
  }

  def write(board: Board, index: Int): Seq[String] = {
    def cellToChar(c: Cell) = c match {
      case Mine => "*"
      case Nearby(nearby) => nearby
    }
    Seq(s"Field #$index:") ++ board.cells.map { cells => cells.map(cellToChar).mkString }
  }
}

case class Board(width: Int, height: Int, cells: IndexedSeq[IndexedSeq[Cell]])
sealed trait Cell
case object Empty extends Cell
case object Mine extends Cell
case class Nearby(value: Int) extends Cell