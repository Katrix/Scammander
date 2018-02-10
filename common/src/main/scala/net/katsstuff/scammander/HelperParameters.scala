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

trait HelperParameters[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra] =>

  /**
    * Many parameters parse Set[A]. This type converts that into a single A.
    * If there is not a single A, the parameter fails.
    */
  case class OnlyOne[A](value: A)
  object OnlyOne {
    implicit def onlyOneValidator[A](implicit validator: UserValidator[A]): UserValidator[OnlyOne[A]] =
      (sender: RootSender) => validator.validate(sender).map(OnlyOne.apply)
  }

  implicit def onlyOneParam[A](implicit setParam: Parameter[Set[A]]): Parameter[OnlyOne[A]] =
    new ProxyParameter[OnlyOne[A], Set[A]] {
      override def param: Parameter[Set[A]] = setParam

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], OnlyOne[A])] = {
        val pos = xs.headOption.map(_.start).getOrElse(-1)
        param.parse(source, extra, xs).flatMap {
          case (rest, seq) if seq.size == 1 => Right((rest, OnlyOne(seq.head)))
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
    ): CommandStep[(List[RawCmdArg], RemainingAsString)] = {
      if (xs.nonEmpty && xs.head.content.nonEmpty) {
        Right((Nil, RemainingAsString(xs.map(_.content).mkString(" "))))
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = Right(Nil)
  }

  /**
    * Parses a given parameter again and again until it fails. Parses at least one.
    */
  case class OneOrMore[A](values: Seq[A])
  object OneOrMore {
    implicit def oneOrMoreParam[A](implicit param: Parameter[A]): Parameter[OneOrMore[A]] =
      new Parameter[OneOrMore[A]] {
        override def name: String = s"${param.name}..."

        override def parse(
            source: RootSender,
            extra: RunExtra,
            xs: List[RawCmdArg]
        ): CommandStep[(List[RawCmdArg], OneOrMore[A])] = {
          @tailrec
          def inner(xs: List[RawCmdArg], acc: Seq[A]): CommandStep[OneOrMore[A]] = {
            if (xs.isEmpty) {
              if (acc.isEmpty) Left(Command.error("Not enough parsed"))
              else Right(OneOrMore(acc))
            } else {
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
        ): Either[List[RawCmdArg], Seq[String]] = {
          @tailrec
          def inner(xs: List[RawCmdArg]): Seq[String] = {
            if (xs.isEmpty) Nil
            else {
              param.suggestions(source, extra, xs) match {
                case Right(suggestions) => suggestions
                case Left(ys)           => inner(ys)
              }
            }
          }

          Right(inner(xs))
        }

        override def usage(source: RootSender): String = s"<$name...>"
      }
  }

  /**
    * Parses a given parameter again and again until it fails. Parses at least zero.
    */
  case class ZeroOrMore[A](values: Seq[A])
  object ZeroOrMore {
    implicit def zeroOrMoreParam[A](implicit param: Parameter[A]): Parameter[ZeroOrMore[A]] =
      new Parameter[ZeroOrMore[A]] {
        override def name: String = s"${param.name}..."

        override def parse(
            source: RootSender,
            extra: RunExtra,
            xs: List[RawCmdArg]
        ): CommandStep[(List[RawCmdArg], ZeroOrMore[A])] = {
          @tailrec
          def inner(xs: List[RawCmdArg], acc: Seq[A]): CommandStep[ZeroOrMore[A]] = {
            if (xs.isEmpty || xs.head.content.isEmpty) {
              Right(ZeroOrMore(acc))
            } else {
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
        ): Either[List[RawCmdArg], Seq[String]] = {
          @tailrec
          def inner(xs: List[RawCmdArg]): Seq[String] = {
            if (xs.isEmpty) Nil
            else {
              param.suggestions(source, extra, xs) match {
                case Right(suggestions) => suggestions
                case Left(ys)           => inner(ys)
              }
            }
          }

          Right(inner(xs))
        }

        override def usage(source: RootSender): String = s"[$name...]"
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

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] =
      param.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = s"[${param.name}]"
  }
}
