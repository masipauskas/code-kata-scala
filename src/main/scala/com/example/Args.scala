package com.example

import com.example.Parsers._

import scala.reflect.ClassTag
import scala.util.Try

class Args {
  def parse(schema: Set[Argument[_]])(args: Seq[String]): Try[Map[String, Option[_]]] = {
    Try {
      val expectedArgs = schema.map { a => (a.name, a) }.toMap[String, Argument[_]]
      val parsed = collection.mutable.Map[String, Option[_]](schema.map(a => (a.name, a.default)).toSeq: _*)
      val it = args.iterator
      while (it.hasNext) {
        val arg = it.next().substring(1)
        val argument = expectedArgs.getOrElse(arg, throw new IllegalArgumentException(s"Unknown argument: -$arg"))
        val value = argument match {
          case flag @ FlagArgument(_) => flag.parse(it)
          case int @ IntArgument(_, _) => int.parse(it)
          case string @ StringArgument(_, _) => string.parse(it)
        }

        parsed.put(arg, value)
      }

      parsed.toMap
    }
  }
}

sealed abstract class Argument[ResultType](val name: String, val default: Option[ResultType], val isFlag: Boolean)
sealed abstract class ValueArgument[ArgumentType](override val name: String, override val default: Option[ArgumentType] = Option.empty)(implicit ct: ClassTag[ArgumentType]) extends Argument[ArgumentType](name, default, isFlag = false)

case class FlagArgument(override val name: String) extends Argument[Boolean](name, Option(false), isFlag = true)
case class IntArgument(override val name: String, override val default: Option[Int] = Option.empty) extends ValueArgument[Int](name, default)
case class StringArgument(override val name: String, override val default: Option[String] = Option.empty) extends ValueArgument[String](name, default)

object Parsers {
  sealed abstract class ArgumentParser[ResultType](arg: Argument[ResultType]) {
    def parse(iterator: Iterator[String]): Option[ResultType]

    protected def parseArgument(iterator: Iterator[String])(p: (String) => ResultType): Option[ResultType] = {
      if (iterator.hasNext) {
        val result = Try(p(iterator.next()))
        Option(result.getOrElse(throw new IllegalArgumentException(s"Unable to parse value for: -${arg.name}")))
      }
      else {
        throw new IllegalArgumentException(s"Unable to parse value for: -${arg.name}")
      }
    }
  }

  implicit class FlagParser(arg: FlagArgument) extends ArgumentParser[Boolean](arg) {
    def parse(iterator: Iterator[String]): Option[Boolean] = Option(true)
  }

  implicit class IntParser(arg: ValueArgument[Int]) extends ArgumentParser[Int](arg) {
    def parse(iterator: Iterator[String]): Option[Int] = parseArgument(iterator)(_.toInt)
  }

  implicit class StringParser(arg: ValueArgument[String]) extends ArgumentParser[String](arg) {
    def parse(iterator: Iterator[String]): Option[String] = parseArgument(iterator)(v => v)
  }
}