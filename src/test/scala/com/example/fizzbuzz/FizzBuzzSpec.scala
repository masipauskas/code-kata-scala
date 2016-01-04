package com.example.fizzbuzz

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by martynas on 04/01/2016.
  */
class FizzBuzzSpec extends FlatSpec with Matchers {
  import FizzBuzz.{run => fRun}
  "run" should "return an empty Seq when n=0" in {
    fRun(0) shouldBe Seq()
  }

  "run" should "return the correct sequence of strings when n=2" in {
    fRun(2) shouldBe Seq("1", "2")
  }

  "run" should "return the correct sequence of strings when n=3 including Fizz" in {
    fRun(3) shouldBe Seq("1", "2", "Fizz")
  }

  "run" should "return the correct sequence of strings when n=5 including Fizz and Buzz" in {
    fRun(5) shouldBe Seq("1", "2", "Fizz", "4", "Buzz")
  }

  "run" should "return the correct sequence of strings when n=15 including Fizz, Buzz and FizzBuzz" in {
    fRun(15) shouldBe Seq("1", "2", "Fizz", "4", "Buzz", "Fizz", "Whizz", "8", "Fizz", "Buzz", "11", "Fizz", "Fizz", "Whizz", "FizzBuzz")
  }
}
