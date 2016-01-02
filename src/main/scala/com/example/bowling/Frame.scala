package com.example.bowling

object Frame {
    def parse(line: String): (Frame, String) = line match {
      case l: String if isStrike(l) => (Strike(), consume(l, 1))
      case l: String if l.length == 1 => (Roll(parseInt(l, 0), 0), consume(l, 1))
      case l: String if isSpare(l) => (Spare(parseInt(l, 0)), consume(l, 2))
      case _ => (Roll(parseInt(line, 0), parseInt(line, 1)), consume(line, 2))
    }

    def parseInt(line: String, pos: Int): Int = {
      if(line.substring(pos, pos + 1) == "-") 0
      else line.substring(pos, pos + 1).toInt
    }

    def consume(line: String, count: Int) = line.substring(count)

    def isStrike(line: String) = line.charAt(0) == 'X'
    def isSpare(line: String) = line.charAt(1) == '/'
}

sealed trait Frame {
  def value: Int
}
case class Roll(firstRoll: Int, secondRoll: Int) extends Frame {
  def value = firstRoll + secondRoll
}
case class Strike() extends Frame {
  def value = 10
}
case class Spare(firstRoll: Int) extends Frame {
  def value = 10
}