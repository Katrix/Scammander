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

import scala.language.higherKinds

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._

trait HelperParameters[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra] =>

  /**
    * Many parameters parse Set[A]. This type converts that into a single A.
    * If there is not a single A, the parameter fails.
    */
  case class OnlyOne[A](value: A)
  object OnlyOne {
    //noinspection ConvertExpressionToSAM
    implicit def onlyOneValidator[A](implicit validator: UserValidator[A]): UserValidator[OnlyOne[A]] =
      new UserValidator[OnlyOne[A]] {
        override def validate(sender: RootSender): F[OnlyOne[A]] =
          validator.validate(sender).map(OnlyOne.apply)
      }
  }

  implicit def onlyOneParam[A](implicit setParam: Parameter[Set[A]]): Parameter[OnlyOne[A]] =
    new ProxyParameter[OnlyOne[A], Set[A]] {
      override def param: Parameter[Set[A]] = setParam

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], OnlyOne[A]] =
        SF.tuple2(ScammanderHelper.getPos, param.parse(source, extra)).transformF { fa =>
          fa.flatMap {
            case (rest, (_, set)) if set.size == 1 => F.pure((rest, OnlyOne(set.head)))
            case (_, (pos, set)) if set.isEmpty =>
              Command.usageErrorF[(List[RawCmdArg], OnlyOne[A])]("No values found", pos)
            case (_, (pos, _)) =>
              Command.usageErrorF[(List[RawCmdArg], OnlyOne[A])]("More than one possible value", pos)
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

    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], RemainingAsString] =
      ScammanderHelper.getArgs
        .flatMapF { xs =>
          if (xs.nonEmpty && xs.head.content.nonEmpty)
            RemainingAsString(xs.map(_.content).mkString(" ")).pure
          else
            ScammanderHelper.notEnoughArgsErrorF[F, RemainingAsString]
        }
        .modify(_ => Nil)

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
      SF.pure(Nil: Seq[String]).modify(_ => Nil: List[RawCmdArg])
  }

  /**
    * Parses a given parameter again and again until it fails. Parses at least one.
    */
  case class OneOrMore[A](values: NonEmptyList[A])
  object OneOrMore {
    implicit def oneOrMoreParam[A](implicit param: Parameter[A]): Parameter[OneOrMore[A]] =
      new Parameter[OneOrMore[A]] {
        override def name: String = s"${param.name}..."

        override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], OneOrMore[A]] = {
          import cats.instances.vector._
          val parse     = param.parse(source, extra)
          val stillMore = ScammanderHelper.getArgs[F].map(_.nonEmpty)

          val res = SF.whileM[Vector, A](stillMore)(parse)

          res.flatMapF { vec =>
            NonEmptyList
              .fromList(vec.toList)
              .fold[F[OneOrMore[A]]](Command.errorF("Not enough parsed"))(nel => F.pure(OneOrMore(nel)))
          }
        }

        override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] = {
          import cats.instances.vector._
          val mkSuggestions = param.suggestions(source, extra)
          val stillMore     = ScammanderHelper.getArgs[F].map(_.nonEmpty)

          SF.whileM[Vector, Seq[String]](stillMore)(mkSuggestions).map(_.lastOption.getOrElse(Nil))
        }

        override def usage(source: RootSender): F[String] = s"<${param.name}...>".pure
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

        override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], ZeroOrMore[A]] = {
          import cats.instances.vector._
          val parse     = param.parse(source, extra)
          val stillMore = ScammanderHelper.getArgs[F].map(_.nonEmpty)

          val res = SF.whileM[Vector, A](stillMore)(parse)

          for {
            xs <- ScammanderHelper.getArgs[F]
            withEmpty <- {
              if (xs.size == 1 && xs.head.content.isEmpty) res.transformF { fa =>
                F.handleError(fa)(_ => (xs, Vector.empty))
              } else res
            }
          } yield ZeroOrMore(withEmpty)
        }

        override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] = {
          import cats.instances.vector._
          val mkSuggestions = param.suggestions(source, extra)
          val stillMore     = ScammanderHelper.getArgs[F].map(_.nonEmpty)

          SF.whileM[Vector, Seq[String]](stillMore)(mkSuggestions).map(_.lastOption.getOrElse(Nil))
        }

        override def usage(source: RootSender): F[String] = s"[${param.name}...]".pure
      }
  }

  implicit def optionParam[A](implicit param: Parameter[A]): Parameter[Option[A]] = new Parameter[Option[A]] {

    override def name: String = param.name

    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], Option[A]] = {
      val parse = param.parse(source, extra).map[Option[A]](Some.apply)
      ScammanderHelper.withFallbackState(parse, SF.pure(None))
    }

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
      param.suggestions(source, extra)

    override def usage(source: RootSender): F[String] = s"[${param.name}]".pure
  }
}
