package com.example

import scala.collection.{mutable, _}
/**
 * Created by Martynas on 28/10/2015.
 */
class BankOCR {
  def process(lines: Iterator[String]): Seq[String] = {
    lines.sliding(3, 4).map(parseLine).toSeq
  }

  def parseLine(lines: Seq[String]): String = {
    require(lines.forall(_.length == lines.head.length), "All 3 lines should be the same length")
    require(lines.head.length % 3 == 0, "All lines should have a length dividable by 3 (all characters must be expected length, i.e. 3")
    require(lines.length == 3, "It should only parse 3 lines at the time")
    val lineLength = lines.head.length
    val linesToParse = lines.map(l => l.sliding(3, 3)).toIndexedSeq
    val accountNumber = mutable.Buffer[(Option[Int], Seq[String])]()
    while(linesToParse.head.nonEmpty) {
      val key = linesToParse.map(f => f.next())
      accountNumber.append((lookupTable.get(key), key))
    }

    val result = new mutable.StringBuilder((lineLength / 3) + 4)
    accountNumber.foreach(n => result.append(n._1.getOrElse("?")))
    if (isIllegible(accountNumber.map(_._1))) {
      result.append(" ILL")
    } else if (isInvalidAccount(accountNumber.map(_._1).toIndexedSeq)) {
      result.append(" ERR")
    }
    result.toString()
  }

  private def isIllegible(accountNumber: Seq[Option[Int]]): Boolean = {
    !accountNumber.forall(_.nonEmpty)
  }

  private def isInvalidAccount(accountNumber: IndexedSeq[Option[Int]]): Boolean = {
    def invalidChecksum() = {
     accountNumber.reverse
       .zipWithIndex
       .map{ case (value, index) => value.get * (index + 1) }
       .sum % 11 != 0
    }

    !accountNumber.forall(_.nonEmpty) || accountNumber.length != 9 || invalidChecksum()
  }

  def hammingDistance(a: Seq[String], b: Seq[String]) = {
    def toCharIterator(s: Seq[String]) = s.flatMap(_.sliding(1, 1))
    toCharIterator(a).zip(toCharIterator(b)).map { case (left, right) => if (left.equals(right)) 1 else 0 }.sum
  }

  private[this] val lookupTable: Map[Seq[String], Int] = Map(
    Seq("   ", "  |", "  |") -> 1,
    Seq(" _ ", " _|", "|_ ") -> 2,
    Seq(" _ ", " _|", " _|") -> 3,
    Seq("   ", "|_|", "  |") -> 4,
    Seq(" _ ", "|_ ", " _|") -> 5,
    Seq(" _ ", "|_ ", "|_|") -> 6,
    Seq(" _ ", "  |", "  |") -> 7,
    Seq(" _ ", "|_|", "|_|") -> 8,
    Seq(" _ ", "|_|", " _|") -> 9,
    Seq(" _ ", "| |", "|_|") -> 0
  )

}