package com.example.leapyear

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by martynas on 06/01/2016.
  */
class LeapYearSpec extends FlatSpec with Matchers {
  import LeapYear.isLeapYear
  "isLeapYear" should "detect 2001 as typical common year" in {
    isLeapYear(2001) shouldBe false
  }

  "isLeapYear" should "detect 1996 as typical leap year" in {
    isLeapYear(1996) shouldBe true
  }

  "isLeapYear" should "detect 1900 as atypical common year" in {
    isLeapYear(1900) shouldBe false
  }

  "isLeapYear" should "detect 2000 as atypical leap year" in {
    isLeapYear(2000) shouldBe true
  }
}
