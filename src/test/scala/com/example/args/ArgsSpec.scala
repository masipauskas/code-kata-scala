package com.example.args

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
 * Created by Martynas on 08/11/2015.
 */
class ArgsSpec extends FlatSpec with Matchers {
  val args = new Args()

  val flagArgument = FlagArgument("l")
  val intArgument = IntArgument("p", default = Option(8000))
  val stringArgument = StringArgument("d", default = Option("/dev/null"))

  "parse" should "return empty argument list, if passed arguments is empty array" in {
    args.parse(Set())(Seq()) shouldEqual Success(Map())
  }

  "parse -> flagArgument" should "return value set to true if argument is being set" in {
    args.parse(Set(flagArgument))(Seq("-l")) shouldEqual Success(Map("l" -> Option(true)))
  }

  "parse -> flagArgument" should "return value set to false if argument is not set" in {
    args.parse(Set(flagArgument))(Seq()) shouldEqual Success(Map("l" -> Option(false)))
  }

  "parse -> intArgument" should "return value set to int if argument is being set" in {
    args.parse(Set(intArgument))(Seq("-p", "8080")) shouldEqual Success(Map("p" -> Option(8080)))
  }

  "parse -> intArgument" should "return value set to default if argument is not set" in {
    args.parse(Set(intArgument))(Seq()) shouldEqual Success(Map("p" -> Option(8000)))
  }

  "parse -> intArgument" should "return failure if argument is set to non-integer" in {
    the [IllegalArgumentException] thrownBy {
      args.parse(Set(intArgument))(Seq("-p", "/dev/null")).get
    } should have message "Unable to parse value for: -p"
  }

  "parse -> stringArgument" should "return value set to int if argument is being set" in {
    args.parse(Set(stringArgument))(Seq("-d", "/usr/logs")) shouldEqual Success(Map("d" -> Option("/usr/logs")))
  }

  "parse -> stringArgument" should "return value set to default if argument is not set" in {
    args.parse(Set(stringArgument))(Seq()) shouldEqual Success(Map("d" -> Option("/dev/null")))
  }

  "parse -> complex arguments" should "correctly parsed command line" in {
    args.parse(Set(stringArgument, intArgument, flagArgument))(Seq(
      "-l",
      "-d", "/usr/logs",
      "-p", "8080"
    )) shouldEqual Success(Map(
      "l" -> Option(true),
      "d" -> Option("/usr/logs"),
      "p" -> Option(8080)
    ))
  }
}
