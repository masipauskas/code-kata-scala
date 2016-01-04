package com.example.fizzbuzz

/**
  * Created by martynas on 04/01/2016.
  */
object FizzBuzz {
  type THandler = (Int) => Option[String]

  val handlers: Seq[THandler] = Seq(
    printFizzForNumbersContainingThree,
    printFizzBuzz,
    printFizz,
    printBuzz,
    printWhizz,
    defaultPrint
  )

  def run(n: Int): Seq[String] = {
    (1 to n).map { v =>
      print(v, handlers)
    }
  }

  def print(n: Int, handlers: Seq[THandler]): String = {
    handlers.find(_(n).isDefined).flatMap(_(n)).getOrElse(s"Undefined: $n")
  }

  def defaultPrint(n: Int) = Option(n.toString)
  def printFizz(n: Int) = if (n % 3 == 0) Option("Fizz") else Option.empty
  def printBuzz(n: Int) = if (n % 5 == 0) Option("Buzz") else Option.empty
  def printFizzBuzz(n: Int) = if (n % 5 == 0 && n % 3 == 0) Option("FizzBuzz") else Option.empty
  def printWhizz(n: Int) = if (n % 7 == 0) Option("Whizz") else Option.empty
  def printFizzForNumbersContainingThree(n: Int) = if (n.toString.contains('3')) Option("Fizz") else Option.empty
}
