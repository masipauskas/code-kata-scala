package com.example.leapyear

/**
  * Created by martynas on 06/01/2016.
  */
object LeapYear {
  def isLeapYear(year: Int): Boolean = {
    val isATypicalLeapYear: (Int) => Boolean = year => (year % 100 == 0) && (year % 400 == 0)
    val isTypicalLeapYear: (Int) => Boolean = year => (year % 4 == 0) && (year % 100 != 0)

    isTypicalLeapYear(year) || isATypicalLeapYear(year)
  }
}
