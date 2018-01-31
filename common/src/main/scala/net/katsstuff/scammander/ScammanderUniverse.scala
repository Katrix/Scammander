/*
 * This file is part of Scammander, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katsstuff.scammander

import java.net.URL
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import java.util.{Locale, UUID}

import scala.annotation.tailrec
import scala.util.Try

import net.katsstuff.scammander
import net.katsstuff.scammander.misc.{HasName, RawCmdArg}
import shapeless._
import shapeless.labelled.FieldType

trait ScammanderUniverse[RootSender, RunExtra, TabExtra, Result]
    extends NormalParametersInstances[RootSender, RunExtra, TabExtra, Result]
    with ParameterLabelledDeriver[RootSender, RunExtra, TabExtra, Result] {

  protected val defaultCommandSuccess: Result

  /**
    * A successful run of a command.
    */
  case class CommandSuccess(result: Result = defaultCommandSuccess)

  /**
    * A typeclass which helps convert a user into another type.
    */
  trait UserValidator[A] {

    /**
      * Validates the sender.
      */
    def validate(sender: RootSender): CommandStep[A]
  }
  object UserValidator {

    /**
      * Create a user validator from a function.
      */
    def mkValidator[A](validator: RootSender => CommandStep[A]): UserValidator[A] =
      (sender: RootSender) => validator(sender)

    implicit val rootValidator: UserValidator[RootSender] = mkValidator(Right.apply)
  }

  //Results and steps

  type CommandFailure = scammander.CommandFailure

  type CommandError = scammander.CommandError
  val CommandError: scammander.CommandError.type = scammander.CommandError

  type CommandSyntaxError = scammander.CommandSyntaxError
  val CommandSyntaxError: scammander.CommandSyntaxError.type = scammander.CommandSyntaxError

  type CommandUsageError = scammander.CommandUsageError
  val CommandUsageError: scammander.CommandUsageError.type = scammander.CommandUsageError

  type MultipleCommandErrors = scammander.MultipleCommandErrors
  val MultipleCommandErrors: scammander.MultipleCommandErrors.type = scammander.MultipleCommandErrors

  type CommandStep[A] = Either[CommandFailure, A]

  //Commands and parameters

  /**
    * The base class for a command. Extend from this if you want more control over a command.
    */
  abstract class Command[Sender, Param](implicit val userValidator: UserValidator[Sender], val par: Parameter[Param]) {

    def run(source: Sender, extra: RunExtra, arg: Param): CommandStep[CommandSuccess]

    def suggestions(source: RootSender, extra: TabExtra, strArgs: List[RawCmdArg]): Seq[String] =
      par.suggestions(source, extra, strArgs)._2

    def usage(source: RootSender): String = par.usage(source)
  }
  object Command {

    /**
      * Create a simple command from a function that takes a parameter of the given type.
      */
    def simple[Param](
        runCmd: (RootSender, RunExtra, Param) => CommandStep[CommandSuccess]
    )(implicit parameter: Parameter[Param]): Command[RootSender, Param] =
      new Command[RootSender, Param] {
        override def run(source: RootSender, extra: RunExtra, arg: Param): CommandStep[CommandSuccess] =
          runCmd(source, extra, arg)
      }

    /**
      * Create a command from a function that takes a parameter and sender of the given types.
      */
    def withSender[Sender, Param](
        runCmd: (Sender, RunExtra, Param) => CommandStep[CommandSuccess]
    )(implicit transformer: UserValidator[Sender], parameter: Parameter[Param]): Command[Sender, Param] =
      new Command[Sender, Param] {
        override def run(source: Sender, extra: RunExtra, arg: Param): CommandStep[CommandSuccess] =
          runCmd(source, extra, arg)
      }

    /**
      * Creates a command success step.
      */
    def successStep(result: Result = defaultCommandSuccess): CommandStep[CommandSuccess] = Right(CommandSuccess(result))

    /**
      * Creates a generic command error step.
      */
    def errorStep(msg: String): CommandStep[CommandSuccess] = Left(CommandError(msg))

    /**
      * Creates a syntax command error step.
      */
    def syntaxErrorStep(msg: String, pos: Int): CommandStep[CommandSuccess] = Left(CommandSyntaxError(msg, pos))

    /**
      * Creates a usage  command error step.
      */
    def usageErrorStep(msg: String, pos: Int): CommandStep[CommandSuccess] = Left(CommandUsageError(msg, pos))

    /**
      * Creates a command success.
      */
    def success(result: Result = defaultCommandSuccess): CommandSuccess = CommandSuccess(result)

    /**
      * Creates a generic command error.
      */
    def error(msg: String): CommandFailure = CommandError(msg)

    /**
      * Creates a syntax command error.
      */
    def syntaxError(msg: String, pos: Int): CommandSyntaxError = CommandSyntaxError(msg, pos)

    /**
      * Creates a usage  command error.
      */
    def usageError(msg: String, pos: Int): CommandUsageError = CommandUsageError(msg, pos)
  }

  /**
    * A parameter for a command. Can convert a list of arguments into a given type.
    * @tparam A The parsed value.
    */
  trait Parameter[A] {

    /**
      * The name of the parameter. Will be used in usage.
      */
    def name: String

    /**
      * Parse a list of arguments into the type of this parameter.
      * @param source The command source.
      * @param extra Extra platform specific info about the command.
      * @param xs The arguments for the command.
      * @return A command step with the remaining arguments, and the parsed type.
      */
    def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)]

    /**
      * Returns the suggestions for a parameter.
      * @param source The command source.
      * @param extra Extra platform specific info about the command.
      * @param xs The arguments for the command.
      * @return A list of the remaining arguments, and the suggestions.
      */
    def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String])

    /**
      * The usage for this command.
      */
    def usage(source: RootSender): String = s"<$name>"
  }

  implicit class OptionOps[A](val option: Option[A]) {
    def toStep(error: => String): CommandStep[A] = option.toRight(CommandError(error))
  }

  /**
    * Represents a parameter that wraps another parameter and transforms the parsed value.
    * @tparam A The new value
    * @tparam B The old value
    */
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      param.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = param.usage(source)
  }

  object Parameter {
    def apply[A](implicit param: Parameter[A]): Parameter[A] = param

    /**
      * Makes a parameter for a type which has names.
      * @param paramName The name of the parameter.
      * @param choices The possible choices for the parameter.
      * @tparam A The type of parameter.
      */
    def mkNamed[A: HasName](paramName: String, choices: => Iterable[A]): Parameter[Set[A]] = new Parameter[Set[A]] {
      override def name: String = paramName

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], Set[A])] = ScammanderHelper.parseMany(name, xs, choices)

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = ScammanderHelper.suggestions(xs, choices)
    }
  }

  //Helper parameters and modifiers

  /**
    * Many parameters parse Set[A]. This type converts that into a single A.
    * If there is not a single A, the parameter fails.
    */
  case class OnlyOne[A](value: A)
  implicit def onlyOneParam[A](implicit setParam: Parameter[Set[A]]): Parameter[A] =
    new ProxyParameter[A, Set[A]] {
      override def param: Parameter[Set[A]] = setParam

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], A)] = {
        val pos = xs.headOption.map(_.start).getOrElse(-1)
        param.parse(source, extra, xs).flatMap {
          case (rest, seq) if seq.size == 1 => Right((rest, seq.head))
          case (_, seq) if seq.isEmpty      => Left(CommandUsageError("No values found", pos))
          case _                            => Left(CommandUsageError("More than one possible value", pos))
        }
      }
    }

  /**
    * Parses the remaining arguments as a single string.
    */
  case class RemainingAsString(string: String) {
    override def toString:               String  = string
    override def hashCode():             Int     = string.hashCode
    override def equals(obj: scala.Any): Boolean = string.equals(obj)
  }

  implicit val remainingAsStringParam: Parameter[RemainingAsString] = new Parameter[RemainingAsString] {
    override def name = "strings..."

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], RemainingAsString)] =
      Right((Nil, RemainingAsString(xs.map(_.content).mkString(" "))))

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (Nil, Nil)
  }

  /**
    * Parses a given parameter again and again until it fails.
    */
  case class AllOff[A](values: Seq[A])
  object AllOff {
    implicit def allOfParam[A](implicit param: Parameter[A]): Parameter[AllOff[A]] = new Parameter[AllOff[A]] {
      override def name: String = s"${param.name}..."

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], AllOff[A])] = {
        @tailrec
        def inner(xs: List[RawCmdArg], acc: Seq[A]): CommandStep[AllOff[A]] = {
          if (xs.isEmpty) Right(AllOff(acc))
          else {
            param.parse(source, extra, xs) match {
              case Right((ys, res)) => inner(ys, acc :+ res)
              case Left(e)          => Left(e)
            }
          }
        }

        inner(xs, Nil).map(Nil -> _)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = {
        @tailrec
        def inner(xs: List[RawCmdArg], finished: Boolean): Seq[String] = {
          if (finished) param.suggestions(source, extra, xs)._2
          else {
            val (ys, suggestions) = param.suggestions(source, extra, xs)

            //FIXME: This breaks for values that don't have any suggestions
            if (suggestions.isEmpty) inner(xs, finished = true) else inner(ys, finished = false)
          }
        }

        (Nil, inner(xs, finished = false))
      }
    }
  }

  /**
    * Parses a flag with a value followed by it.
    * @param value The value if it was present.
    * @tparam Name The name of the flag.
    * @tparam A The type of the value.
    */
  case class ValueFlag[Name <: String, A](value: Option[A])
  implicit def valueFlagParameter[Name <: String, A](
      implicit witness: Witness.Aux[Name],
      flagParam: Parameter[A]
  ): Parameter[ValueFlag[Name, A]] =
    new Parameter[ValueFlag[Name, A]] {
      private val flagName = if (witness.value.size > 1) s"--${witness.value}" else s"-${witness.value}"
      override def name: String = s"$flagName ${flagParam.name}"

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], ValueFlag[Name, A])] = {
        def inner(ys: List[RawCmdArg], acc: List[RawCmdArg]): CommandStep[(List[RawCmdArg], ValueFlag[Name, A])] = {
          if (ys.nonEmpty) {
            val h = ys.head
            if (h.content == flagName) {
              flagParam.parse(source, extra, ys.tail).map(t => (acc reverse_::: t._1, ValueFlag(Some(t._2))))
            } else inner(ys.tail, h :: acc)
          } else Right((acc.reverse, ValueFlag(None)))
        }

        inner(xs, Nil)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) =
        if (xs.lengthCompare(2) == 0) {
          flagParam.suggestions(source, extra, xs.drop(1))
        } else (xs.drop(2), Nil)
    }

  /**
    * Parses a flag.
    * @param present If the flag was present.
    * @tparam Name The name of the flag.
    */
  case class BooleanFlag[Name <: String](present: Boolean)
  implicit def booleanFlagParameter[Name <: String](implicit witness: Witness.Aux[Name]): Parameter[BooleanFlag[Name]] =
    new Parameter[BooleanFlag[Name]] {
      private val flagName = if (witness.value.size > 1) s"--${witness.value}" else s"-${witness.value}"
      override def name: String = flagName

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], BooleanFlag[Name])] = {
        def inner(ys: List[RawCmdArg], acc: List[RawCmdArg]): CommandStep[(List[RawCmdArg], BooleanFlag[Name])] = {
          if (ys.nonEmpty) {
            val h = ys.head
            if (h.content == flagName) {
              Right((ys.tail reverse_::: acc, BooleanFlag(true)))
            } else inner(ys.tail, h :: acc)
          } else Right((acc.reverse, BooleanFlag(false)))
        }

        inner(xs, Nil)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = (xs.drop(1), Nil)
    }

  /**
    * A helper to group flags together with normal parameters.
    * @param flags The flags values.
    * @param parameters The parameter values.
    * @tparam A The flag parameters.
    * @tparam B The other value parameters.
    */
  case class Flags[A, B](flags: A, parameters: B)
  implicit def flagsParameter[A, B](
      implicit flagsParam: Parameter[A],
      paramParam: Parameter[B]
  ): Parameter[Flags[A, B]] = new Parameter[Flags[A, B]] {
    override def name: String = s"${paramParam.name} ${flagsParam.name}"

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Flags[A, B])] = {
      println(s"Start $xs")
      for {
        t1 <- flagsParam.parse(source, extra, xs)
        _ = println(s"T1 $t1")
        t2 <- paramParam.parse(source, extra, t1._1)
        _ = println(s"T2 $t2")
      } yield t2._1 -> Flags(t1._2, t2._2)
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = {
      val (ys, flagSuggestions) = flagsParam.suggestions(source, extra, xs)
      if (flagSuggestions.isEmpty) paramParam.suggestions(source, extra, ys) else ys -> flagSuggestions
    }
  }

  implicit def optionParam[A](implicit param: Parameter[A]): Parameter[Option[A]] = new Parameter[Option[A]] {

    override def name: String = param.name

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Option[A])] = {
      val some = param.parse(source, extra, xs).map(t => t._1 -> Some(t._2))
      val none = Right(xs -> None)

      some.fold(_ => none, Right.apply)
    }

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      param.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = s"[${param.name}]"
  }

  //Or parameter

  /**
    * A class which can parse a normal parameter, or can optionally be filled
    * by some other context.
    * @param value The parsed value.
    * @tparam Base The type to parse.
    * @tparam Context The context type which specifies how to parse the value
    *                 if it's not present.
    */
  case class Or[Base, Context](value: Base)

  /**
    * Used in [[Or]]. Parse a value, or return the sender as that value.
    * Requires that a [[UserValidator]] is present for that type.
    */
  sealed trait Source
  type OrSource[Base] = Base Or Source

  implicit def orSource[Base](
      implicit parameter: Parameter[Base],
      validator: UserValidator[Base]
  ): Parameter[OrSource[Base]] =
    new ProxyParameter[OrSource[Base], Base] {
      override def param: Parameter[Base] = parameter

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], OrSource[Base])] = {
        val res = for {
          e1 <- param.parse(source, extra, xs).left
          e2 <- validator.validate(source).map(xs -> _).left
        } yield e1.merge(e2)

        res.map(t => t._1 -> Or(t._2))
      }

      override def usage(source: RootSender): String =
        validator.validate(source).map(_ => s"[$name]").getOrElse(super.usage(source))
    }

  /**
    * Given some parsed time, alternatively returns now instead.
    */
  sealed trait Now
  type OrNow[Base] = Base Or Now
  implicit val dateTimeOrNowParam: Parameter[LocalDateTime Or Now] = new Parameter[LocalDateTime Or Now] {
    override def name: String = dateTimeParam.name
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], LocalDateTime Or Now)] = {
      val (ys, res) = dateTimeParam.parse(source, extra, xs).getOrElse((xs, LocalDateTime.now))
      Right((ys, Or(res)))
    }

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      dateTimeParam.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = s"[$name]"
  }
}

trait NormalParametersInstances[RootSender, RunExtra, TabExtra, Result] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra, Result] =>
  def primitiveParam[A](parName: String, s: String => A): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        if (xs.nonEmpty)
          Try(s(xs.head.content))
            .map(xs.tail -> _)
            .toEither
            .left
            .map(e => CommandSyntaxError(e.getMessage, xs.head.start))
        else Left(ScammanderHelper.notEnoughArgs)

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = (xs.tail, Nil)
    }

  def mkSingle[A](
      parName: String,
      parser: String => CommandStep[A],
      possibleSuggestions: () => Seq[String]
  ): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        if (xs.nonEmpty) parser(xs.head.content).map(xs.tail -> _)
        else Left(ScammanderHelper.notEnoughArgs)

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = ScammanderHelper.suggestions(xs, possibleSuggestions())
    }

  implicit val byteParam:   Parameter[Byte]    = primitiveParam("byte", _.toByte)
  implicit val shortParam:  Parameter[Short]   = primitiveParam("short", _.toShort)
  implicit val intParam:    Parameter[Int]     = primitiveParam("int", _.toInt)
  implicit val longParam:   Parameter[Long]    = primitiveParam("long", _.toLong)
  implicit val floatParam:  Parameter[Float]   = primitiveParam("float", _.toFloat)
  implicit val doubleParam: Parameter[Double]  = primitiveParam("double", _.toDouble)
  implicit val boolParam:   Parameter[Boolean] = primitiveParam("boolean", _.toBoolean)
  implicit val strParam:    Parameter[String]  = primitiveParam("string", identity)
  implicit val unitParam:   Parameter[Unit]    = primitiveParam("", _ => ())

  implicit val urlParam: Parameter[URL] = new Parameter[URL] {
    override def name: String = "url"

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], URL)] = {
      if (xs.nonEmpty) {
        val RawCmdArg(pos, _, arg) = xs.head
        Try(new URL(arg))
          .flatMap { url =>
            Try {
              url.toURI
              xs.tail -> url
            }
          }
          .toEither
          .left
          .map(e => CommandSyntaxError(e.getMessage, pos))
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs.tail, Nil)
  }

  implicit val bigDecimalParam: Parameter[BigDecimal] = primitiveParam("bigDecimal", BigDecimal.apply)
  implicit val bigIntParam:     Parameter[BigInt]     = primitiveParam("bigInt", BigInt.apply)

  implicit val uuidParam: Parameter[UUID] = primitiveParam("uuid", UUID.fromString)

  implicit val dateTimeParam: Parameter[LocalDateTime] = new Parameter[LocalDateTime] {
    override def name: String = "dataTime"
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], LocalDateTime)] = {
      if (xs.nonEmpty) {
        val RawCmdArg(pos, _, arg) = xs.head
        Try(LocalDateTime.parse(arg))
          .recoverWith {
            case _: DateTimeParseException =>
              Try(LocalDateTime.of(LocalDate.now, LocalTime.parse(arg)))
          }
          .recoverWith {
            case _: DateTimeParseException => Try(LocalDateTime.of(LocalDate.parse(arg), LocalTime.MIDNIGHT))
          }
          .toEither
          .left
          .map { _ =>
            CommandSyntaxError("Invalid date-time!", pos)
          }
          .map(xs.tail -> _)
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = {
      val date = LocalDateTime.now.withNano(0).toString
      val arg  = xs.headOption.map(_.content).getOrElse("")

      xs.tail -> (if (date.startsWith(arg)) Seq(date) else Nil)
    }
  }

  implicit val durationParam: Parameter[Duration] = new Parameter[Duration] {
    override def name: String = "duration"
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Duration)] = {
      if (xs.nonEmpty) {
        val RawCmdArg(pos, _, arg) = xs.head
        val s                      = arg.toUpperCase(Locale.ROOT)

        val usedS = if (!s.contains("T")) {
          val s1 = if (s.contains("D")) {
            if (s.contains("H") || s.contains("M") || s.contains("S")) s.replace("D", "DT")
            else if (s.startsWith("P")) "PT" + s.substring(1)
            else "T" + s
          } else s
          if (!s1.startsWith("P")) "P" + s1 else s1
        } else s

        Try(Duration.parse(usedS)).toEither.left
          .map(e => CommandSyntaxError(e.getMessage, pos))
          .map(xs.tail -> _)
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs.tail, Nil)
  }
}

trait ParameterLabelledDeriver[RootSender, RunExtra, TabExtra, Result]
    extends ParameterDeriver[RootSender, RunExtra, TabExtra, Result] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra, Result] =>

  implicit def genParam[A, Gen](
      implicit gen: LabelledGeneric.Aux[A, Gen],
      genParam: Lazy[Parameter[Gen]]
  ): Parameter[A] =
    new ProxyParameter[A, Gen] {
      override def param: Parameter[Gen] = genParam.value

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        genParam.value.parse(source, extra, xs).map(t => t._1 -> gen.from(t._2))
    }

  implicit def hConsLabelledParam[HK <: Symbol, HV, T <: HList](
      implicit
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :: T] =
    new Parameter[FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name} ${tParam.value.name}"

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], ::[FieldType[HK, HV], T])] = {
        for {
          t1 <- hParam.value.parse(source, extra, xs)
          t2 <- tParam.value.parse(source, extra, t1._1)
        } yield (t2._1, labelled.field[HK](t1._2) :: t2._2)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, extra, xs)
        val (rest, t) = tParam.value.suggestions(source, extra, ys)

        (rest, h ++ t)
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hName.value.name
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) s"<$hUsage>" else s"<$hUsage> $tUsage"
      }
    }

  implicit def cConsLabelledParam[HK <: Symbol, HV, T <: Coproduct](
      implicit
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :+: T] =
    new Parameter[FieldType[HK, HV] :+: T] {
      override def name: String = s"${hName.value.name}|${tParam.value.name}"

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], FieldType[HK, HV] :+: T)] = {
        for {
          e1 <- hParam.value.parse(source, extra, xs).map { case (ys, h) => ys -> Inl(labelled.field[HK](h)) }.left
          e2 <- tParam.value.parse(source, extra, xs).map { case (ys, t) => ys -> Inr(t) }.left
        } yield e1.merge(e2)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = {
        val (hRest, h) = hParam.value.suggestions(source, extra, xs)
        val (tRest, t) = tParam.value.suggestions(source, extra, xs)

        val rest = if (hRest.lengthCompare(tRest.size) > 0) hRest else tRest
        (rest, h ++ t)
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) s"($hUsage)" else s"($hUsage)|$tUsage"
      }
    }
}

trait ParameterDeriver[RootSender, RunExtra, TabExtra, Result] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra, Result] =>
  implicit def hConsParam[H, T <: HList](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :: T] =
    new Parameter[H :: T] {
      override def name: String = s"${hParam.value.name} ${tParam.value.name}"

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], ::[H, T])] = {
        for {
          t1 <- hParam.value.parse(source, extra, xs)
          t2 <- tParam.value.parse(source, extra, t1._1)
        } yield (t1._1, t1._2 :: t2._2)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, extra, xs)
        val (rest, t) = tParam.value.suggestions(source, extra, ys)

        (rest, h ++ t)
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) hUsage else s"$hUsage $tUsage"
      }
    }

  implicit val hNilParam: Parameter[HNil] = new Parameter[HNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], HNil)] =
      Right((xs, HNil))

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs, Nil)

    override def usage(source: RootSender): String = ""
  }

  implicit def cConsParam[H, T <: Coproduct](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :+: T] =
    new Parameter[H :+: T] {
      override def name: String = s"${hParam.value.name}|${tParam.value.name}"

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], :+:[H, T])] = {
        for {
          e1 <- hParam.value.parse(source, extra, xs).map { case (ys, h) => ys -> Inl(h) }.left
          e2 <- tParam.value.parse(source, extra, xs).map { case (ys, t) => ys -> Inr(t) }.left
        } yield e1.merge(e2)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = {
        val (hRest, h) = hParam.value.suggestions(source, extra, xs)
        val (tRest, t) = tParam.value.suggestions(source, extra, xs)

        val rest = if (hRest.lengthCompare(tRest.size) > 0) hRest else tRest
        (rest, h ++ t)
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) hUsage else s"$hUsage|$tUsage"
      }
    }

  implicit val cNilParam: Parameter[CNil] = new Parameter[CNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], CNil)] =
      sys.error("CNil")

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs, Nil)

    override def usage(source: RootSender): String = ""
  }
}
