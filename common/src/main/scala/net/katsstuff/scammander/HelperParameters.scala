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

import cats.data.NonEmptyList
import cats.syntax.all._

trait HelperParameters { self: ScammanderBase =>

  /**
    * Many parameters parse Set[A]. This type converts that into a single A.
    * If there is not a single A, the parameter fails.
    */
  case class OnlyOne[A](value: A)
  object OnlyOne {
    //noinspection ConvertExpressionToSAM
    implicit def onlyOneValidator[A](implicit validator: UserValidator[A]): UserValidator[OnlyOne[A]] =
      new UserValidator[OnlyOne[A]] {
        override def validate[F[_]: ParserError](sender: RootSender): F[OnlyOne[A]] =
          validator.validate(sender).map(OnlyOne.apply)
      }
  }

  implicit def onlyOneParam[A](implicit setParam: Parameter[Set[A]]): Parameter[OnlyOne[A]] =
    new ProxyParameter[OnlyOne[A], Set[A]] {
      override def param: Parameter[Set[A]] = setParam

      override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[OnlyOne[A]] =
        for {
          pos    <- ScammanderHelper.getPos
          parsed <- setParam.parse(source, extra)
          res <- parsed.toSeq match {
            case Seq()       => Result.usageErrorF[F, OnlyOne[A]]("No values found", pos)
            case Seq(single) => OnlyOne(single).pure
            case Seq(_)      => Result.usageErrorF[F, OnlyOne[A]]("More than one possible value", pos)
          }
        } yield res
    }

  /**
    * Parses the remaining arguments as a single string.
    */
  case class RemainingAsString(string: String) {
    override def toString: String = string
  }

  implicit val remainingAsStringParam: Parameter[RemainingAsString] = new Parameter[RemainingAsString] {
    override def name = "strings..."

    override def parse[F[_]](
        source: RootSender,
        extra: RunExtra
    )(implicit S: ParserState[F], E: ParserError[F]): F[RemainingAsString] =
      S.get
        .flatMap { xs =>
          if (xs.nonEmpty && xs.head.content.nonEmpty)
            RemainingAsString(xs.map(_.content).mkString(" ")).pure
          else
            ScammanderHelper.notEnoughArgsErrorF[F, RemainingAsString]
        } <* S.set(Nil)

    override def suggestions[F[_]](
        source: RootSender,
        extra: TabExtra
    )(implicit S: ParserState[F], E: ParserError[F]): F[Seq[String]] =
      S.set(Nil).as(Nil)
  }

  /**
    * Parses a given parameter again and again until it fails. Parses at least one.
    */
  case class OneOrMore[A](values: NonEmptyList[A])
  object OneOrMore {
    implicit def oneOrMoreParam[A](implicit param: Parameter[A]): Parameter[OneOrMore[A]] =
      new Parameter[OneOrMore[A]] {
        override val name: String = s"${param.name}..."

        override def parse[F[_]](
            source: RootSender,
            extra: RunExtra
        )(implicit S: ParserState[F], E: ParserError[F]): F[OneOrMore[A]] = {
          import cats.instances.vector._
          val parse     = param.parse(source, extra)
          val stillMore = S.get.map(_.nonEmpty)

          val parseMany = E.whileM[Vector, A](stillMore)(parse)

          parseMany.flatMap { vec =>
            NonEmptyList
              .fromList(vec.toList)
              .fold[F[OneOrMore[A]]](Result.errorF("Not enough parsed"))(OneOrMore(_).pure[F])
          }
        }

        override def suggestions[F[_]](
            source: RootSender,
            extra: TabExtra
        )(implicit S: ParserState[F], E: ParserError[F]): F[Seq[String]] = {
          import cats.instances.vector._
          val mkSuggestions = param.suggestions(source, extra)
          val stillMore     = S.get.map(_.nonEmpty)

          E.whileM[Vector, Seq[String]](stillMore)(mkSuggestions).map(_.lastOption.getOrElse(Nil))
        }

        override def usage[F[_]: ParserError](source: RootSender): F[String] = s"<${param.name}...>".pure
      }
  }

  /**
    * Parses a given parameter again and again until it fails. Parses at least zero.
    */
  case class ZeroOrMore[A](values: Seq[A])
  object ZeroOrMore {
    implicit def zeroOrMoreParam[A](implicit param: Parameter[A]): Parameter[ZeroOrMore[A]] =
      new Parameter[ZeroOrMore[A]] {
        override val name: String = s"${param.name}..."

        override def parse[F[_]](
            source: RootSender,
            extra: RunExtra
        )(implicit S: ParserState[F], E: ParserError[F]): F[ZeroOrMore[A]] = {
          import cats.instances.vector._
          val parse     = param.parse(source, extra)
          val stillMore = S.get.map(_.nonEmpty)

          val parseSome = E.whileM[Vector, A](stillMore)(parse)

          for {
            xs <- S.get
            withEmpty <- if ((xs.size == 1 && xs.head.content.isEmpty) || xs.isEmpty) Vector.empty.pure[F]
            else parseSome
          } yield ZeroOrMore(withEmpty)
        }

        override def suggestions[F[_]](
            source: RootSender,
            extra: TabExtra
        )(implicit S: ParserState[F], E: ParserError[F]): F[Seq[String]] = {
          import cats.instances.vector._
          val mkSuggestions = param.suggestions(source, extra)
          val stillMore     = S.get.map(_.nonEmpty)

          E.whileM[Vector, Seq[String]](stillMore)(mkSuggestions).map(_.lastOption.getOrElse(Nil))
        }

        override def usage[F[_]: ParserError](source: RootSender): F[String] = s"[${param.name}...]".pure
      }
  }

  implicit def optionParam[A](implicit param: Parameter[A]): Parameter[Option[A]] = new Parameter[Option[A]] {

    override val name: String = param.name

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[Option[A]] = {
      val parse = param.parse(source, extra).map(_.some)
      ScammanderHelper.withFallback(parse, (None: Option[A]).pure)
    }

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      param.suggestions(source, extra)

    override def usage[F[_]: ParserError](source: RootSender): F[String] = s"[${param.name}]".pure
  }
}
