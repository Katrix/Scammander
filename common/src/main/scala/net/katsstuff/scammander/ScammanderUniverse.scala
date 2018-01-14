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
import net.katsstuff.scammander.misc.{HasName, MkHListWitness, RawCmdArg}
import shapeless._
import shapeless.labelled.FieldType

trait ScammanderUniverse[RootSender, RunExtra, TabExtra]
    extends NormalParametersInstances[RootSender, RunExtra, TabExtra]
    with ParameterLabelledDeriver[RootSender, RunExtra, TabExtra] {

  trait UserValidator[A] {

    def validate(sender: RootSender): CommandStep[A]
  }
  object UserValidator {
    def mkValidator[A](validator: RootSender => CommandStep[A]): UserValidator[A] =
      (sender: RootSender) => validator(sender)

    implicit val rootValidator: UserValidator[RootSender] = mkValidator(Right.apply)
  }

  //Results and steps

  type CommandResult = scammander.CommandResult
  val CommandResult: scammander.CommandResult.type = scammander.CommandResult

  type CommandSuccess = scammander.CommandSuccess
  val CommandSuccess: scammander.CommandSuccess.type = scammander.CommandSuccess

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

  abstract class Command[Sender, Param](implicit val userValidator: UserValidator[Sender], val par: Parameter[Param]) {

    def run(source: Sender, extra: RunExtra, arg: Param): CommandResult

    def suggestions(source: RootSender, extra: TabExtra, strArgs: List[RawCmdArg]): Seq[String] =
      par.suggestions(source, extra, strArgs)._2

    def usage(source: RootSender): String = par.usage(source)
  }
  object Command {
    def simple[Param](
        runCmd: (RootSender, RunExtra, Param) => CommandResult
    )(implicit parameter: Parameter[Param]): Command[RootSender, Param] =
      new Command[RootSender, Param] {
        override def run(source: RootSender, extra: RunExtra, arg: Param): CommandResult = runCmd(source, extra, arg)
      }

    def withSender[Sender, Param](
        runCmd: (Sender, RunExtra, Param) => CommandResult
    )(implicit transformer: UserValidator[Sender], parameter: Parameter[Param]): Command[Sender, Param] =
      new Command[Sender, Param] {
        override def run(source: Sender, extra: RunExtra, arg: Param): CommandResult = runCmd(source, extra, arg)
      }

    def success(count: Int = 1):            CommandSuccess     = CommandSuccess(count)
    def error(msg: String):                 CommandError       = CommandError(msg)
    def syntaxError(msg: String, pos: Int): CommandSyntaxError = CommandSyntaxError(msg, pos)
    def usageError(msg: String, pos: Int):  CommandUsageError  = CommandUsageError(msg, pos)
  }

  trait Parameter[A] {

    def name: String

    def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)]

    def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String])

    def usage(source: RootSender): String = s"<$name>"
  }
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      param.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = param.usage(source)
  }

  object Parameter {
    def apply[A](implicit param: Parameter[A]): Parameter[A] = param

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

  case class Named[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S]) extends ProxyParameter[A, A] {
    override def name: String = w.value

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
      param.parse(source, extra, xs)
  }

  case class Choices(name: String, choices: Set[String], sendValid: Boolean = false) extends Parameter[String] {
    private val choiceMap = choices.map(_.toLowerCase(Locale.ROOT)).map(s => s -> s).toMap

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], String)] = {
      val res = ScammanderHelper.parse("choice", xs, choiceMap)
      if (sendValid) {
        val head = xs.head.content
        res.left.map {
          case CommandUsageError(_, pos) =>
            CommandUsageError(s"$head is not a valid parameter.\nValid parameters: ${choices.mkString(", ")}", pos)
          case other => other
        }
      } else res
    }

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      ScammanderHelper.suggestions(xs, choices)
  }

  class ChoicesT[Name <: String, L <: HList, SendValid <: Boolean](
      implicit nameW: Witness.Aux[Name],
      mkHList: MkHListWitness[L],
      toTraversable: ops.hlist.ToTraversable.Aux[L, Set, String],
      sendValidW: Witness.Aux[SendValid]
  ) extends ProxyParameter[String, String] {
    private val choices: Set[String] = toTraversable(mkHList.value)

    override val param: Parameter[String] = Choices(nameW.value, choices, sendValidW.value)

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], String)] =
      param.parse(source, extra, xs)
  }

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
              flagParam.parse(source, extra, ys.tail).map(t => (acc ::: t._1, ValueFlag(Some(t._2))))
            } else inner(ys.tail, h :: acc)
          } else Right((acc, ValueFlag(None)))
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
              Right((ys.tail ::: acc, BooleanFlag(true)))
            } else inner(ys.tail, h :: acc)
          } else Right((acc, BooleanFlag(false)))
        }

        inner(xs, Nil)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) = (xs.drop(1), Nil)
    }

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
    ): CommandStep[(List[RawCmdArg], Flags[A, B])] =
      for {
        t1 <- flagsParam.parse(source, extra, xs)
        t2 <- paramParam.parse(source, extra, t1._1)
      } yield t2._1 -> Flags(t1._2, t2._2)

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = {
      val (ys, flagSuggestions) = flagsParam.suggestions(source, extra, xs)
      if (flagSuggestions.isEmpty) paramParam.suggestions(source, extra, ys) else ys -> flagSuggestions
    }
  }

  /*TODO: Find way to use Option class instead
    class Optional[A]
    class OptionalWeak[A]
   */

  //Or parameter
  case class Or[Base, TargetType](value: Base)
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
        val res = param.parse(source, extra, xs).left.flatMap { e1 =>
          validator.validate(source).map(xs -> _).left.map(e2 => e1.merge(e2))
        }

        res.map(t => t._1 -> Or(t._2))
      }

      override def usage(source: RootSender): String =
        validator.validate(source).map(_ => s"[$name]").getOrElse(super.usage(source))
    }

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

trait NormalParametersInstances[RootSender, RunExtra, TabExtra] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra] =>
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

trait ParameterLabelledDeriver[RootSender, RunExtra, TabExtra]
    extends ParameterDeriver[RootSender, RunExtra, TabExtra] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra] =>

  implicit def genParam[A, Gen](implicit gen: LabelledGeneric.Aux[A, Gen], genParam: Parameter[Gen]): Parameter[A] =
    new ProxyParameter[A, Gen] {
      override def param: Parameter[Gen] = genParam

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        genParam.parse(source, extra, xs).map(t => t._1 -> gen.from(t._2))
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
        } yield (Nil, labelled.field[HK](t1._2) :: t2._2)
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
    }
}

trait ParameterDeriver[RootSender, RunExtra, TabExtra] { self: ScammanderUniverse[RootSender, RunExtra, TabExtra] =>
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
        } yield (Nil, t1._2 :: t2._2)
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
    }

  implicit val hNilParam: Parameter[HNil] = new Parameter[HNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], HNil)] =
      Right((xs, HNil))

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs, Nil)
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
    }

  implicit val cNilParam: Parameter[CNil] = new Parameter[CNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], CNil)] =
      sys.error("CNil")

    override def suggestions(source: RootSender, extra: TabExtra, xs: List[RawCmdArg]): (List[RawCmdArg], Seq[String]) =
      (xs, Nil)
  }
}
