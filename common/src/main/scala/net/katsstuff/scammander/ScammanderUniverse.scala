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

import scala.annotation.tailrec
import scala.util.Try

import net.katsstuff.scammander
import net.katsstuff.scammander.misc.{MkHListWitness, RawCmdArg}
import shapeless.labelled.FieldType
import shapeless._

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
  }

  //Helper parameters and modifiers

  case class Named[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S]) extends ProxyParameter[A, A] {
    override def name: String = w.value

    override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
      param.parse(source, extra, xs)
  }

  case class Choices(name: String, choices: Set[String], sendValid: Boolean = false) extends Parameter[String] {
    private val choiceMap = choices.map(s => s -> s).toMap

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
  object OnlyOne {
    implicit def parameter[A](implicit setParam: Parameter[Iterable[A]]): Parameter[A] =
      new ProxyParameter[A, Iterable[A]] {
        override def param: Parameter[Iterable[A]] = setParam

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
}

trait NormalParametersInstances[RootSender, RunExtra, TabExtra] {
  self: ScammanderUniverse[RootSender, RunExtra, TabExtra] =>
  def primitivePar[A](parName: String, s: String => A): Parameter[A] =
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

  implicit val bytePar:     Parameter[Byte]    = primitivePar("byte", _.toByte)
  implicit val shortPar:    Parameter[Short]   = primitivePar("short", _.toShort)
  implicit val intPar:      Parameter[Int]     = primitivePar("int", _.toInt)
  implicit val longPar:     Parameter[Long]    = primitivePar("long", _.toLong)
  implicit val floatPar:    Parameter[Float]   = primitivePar("float", _.toFloat)
  implicit val doubleParam: Parameter[Double]  = primitivePar("double", _.toDouble)
  implicit val boolPar:     Parameter[Boolean] = primitivePar("boolean", _.toBoolean)
  implicit val strPar:      Parameter[String]  = primitivePar("string", identity)
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
