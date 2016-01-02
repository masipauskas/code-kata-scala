package com.example.bowling

import scala.annotation.tailrec

object BowlingGame {
  def score(line: String): Int = {
    val parsedLine = parseLine(line)
    calculateScore(parsedLine)
  }

  def parseLine(line: String): Seq[Frame] = {
    import Frame.parse

    @tailrec
    def parseR(l: String, r: Seq[Frame] = Seq()): Seq[Frame] = {
      if (l.isEmpty) r
      else {
        val (roll, left) = parse(l)
        parseR(left, r :+ roll)
      }
    }

    parseR(line)
  }

  def calculateScore(line: Seq[Frame]): Int = {
    def firstRollValue(r: Frame) = r match {
      case Roll(firstRoll, _) => firstRoll
      case Spare(firstRoll) => firstRoll
      case Strike() => 10
    }

    val (totalScore, _) = line.tails.foldLeft((0, 0)) { case ((count, frame), rolls) =>
        if (frame < 10 && rolls.nonEmpty) {
          val value = rolls.head match {
            case r: Roll => r.value
            case s: Strike =>
              s.value + (rolls.tail.head match {
                case s: Strike => s.value + firstRollValue(rolls.tail.tail.head)
                case other: Frame => other.value
              })
            case s: Spare => s.value + firstRollValue(rolls.tail.head)
          }

          (count + value, frame + 1)
        } else (count, frame)
    }

    totalScore
  }
}