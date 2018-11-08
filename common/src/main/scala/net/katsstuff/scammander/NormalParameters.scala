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

import java.net.URL
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import java.util.{Locale, UUID}

import scala.util.Try

import cats.syntax.all._
import cats.{Eval, Monad}

trait NormalParameters { self: ScammanderBase =>

  def primitiveParam[A](parName: String, s: String => A): Parameter[A] = new Parameter[A] {
    override val name: String = parName

    override def parse[F[_]: Monad: ParserState](source: RootSender, extra: RunExtra)(
        implicit E: ParserError[F]
    ): F[A] =
      for {
        arg <- ScammanderHelper.firstArgAndDrop
        res <- E.catchNonFatal(s(arg.content)) { _ =>
          Result.syntaxErrorNel(s"${arg.content} is not a valid $name", arg.start)
        }
      } yield res

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      ScammanderHelper.dropFirstArg
  }

  def mkSingle[A](parName: String, parser: String => A, possibleSuggestions: Eval[Seq[String]]): Parameter[A] =
    new Parameter[A] {

      override val name: String = parName

      override def parse[F[_]: Monad: ParserState: ParserError](source: RootSender, extra: RunExtra): F[A] =
        ScammanderHelper.firstArgAndDrop.map(arg => parser(arg.content))

      override def suggestions[F[_]: Monad: ParserState: ParserError](
          source: RootSender,
          extra: TabExtra
      ): F[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), possibleSuggestions)
    }

  implicit val byteParam: Parameter[Byte]     = primitiveParam("byte", _.toByte)
  implicit val shortParam: Parameter[Short]   = primitiveParam("short", _.toShort)
  implicit val intParam: Parameter[Int]       = primitiveParam("int", _.toInt)
  implicit val longParam: Parameter[Long]     = primitiveParam("long", _.toLong)
  implicit val floatParam: Parameter[Float]   = primitiveParam("float", _.toFloat)
  implicit val doubleParam: Parameter[Double] = primitiveParam("double", _.toDouble)
  implicit val boolParam: Parameter[Boolean]  = primitiveParam("boolean", _.toBoolean)
  implicit val stringParam: Parameter[String] = primitiveParam("string", identity)

  implicit val unitParam: Parameter[Unit] = new Parameter[Unit] {
    override def name: String = ""

    override def parse[F[_]: Monad: ParserState: ParserError](source: RootSender, extra: RunExtra): F[Unit] = ().pure

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      (Nil: Seq[String]).pure
  }

  implicit val urlParam: Parameter[URL] = new Parameter[URL] {

    override val name: String = "url"

    override def parse[F[_]: Monad: ParserState](source: RootSender, extra: RunExtra)(
        implicit E: ParserError[F]
    ): F[URL] =
      for {
        arg <- ScammanderHelper.firstArgAndDrop
        url <- E.catchNonFatal(new URL(arg.content))(e => Result.syntaxErrorNel(e.getMessage, arg.start))
        res <- E.catchNonFatal {
          url.toURI //Checks validity
          url
        } { e =>
          Result.syntaxErrorNel(e.getMessage, arg.start)
        }
      } yield res

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      ScammanderHelper.dropFirstArg
  }

  implicit val bigDecimalParam: Parameter[BigDecimal] = primitiveParam("decimal number", BigDecimal.apply)
  implicit val bigIntParam: Parameter[BigInt]         = primitiveParam("integer number", BigInt.apply)

  implicit val uuidParam: Parameter[UUID] = primitiveParam("uuid", UUID.fromString)

  implicit val dateTimeParam: Parameter[LocalDateTime] = new Parameter[LocalDateTime] {

    override val name: String = "dataTime"

    override def parse[F[_]: Monad: ParserState](source: RootSender, extra: RunExtra)(
        implicit E: ParserError[F]
    ): F[LocalDateTime] =
      for {
        arg <- ScammanderHelper.firstArgAndDrop
        res <- E.catchNonFatal(
          Try(LocalDateTime.parse(arg.content))
            .recoverWith {
              case _: DateTimeParseException =>
                Try(LocalDateTime.of(LocalDate.now, LocalTime.parse(arg.content)))
            }
            .recoverWith {
              case _: DateTimeParseException =>
                Try(LocalDateTime.of(LocalDate.parse(arg.content), LocalTime.MIDNIGHT))
            }
            .get
        ) { _ =>
          Result.syntaxErrorNel("Invalid date-time", arg.start)
        }
      } yield res

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      for {
        arg <- ScammanderHelper.firstArgOpt.map(_.fold("")(_.content))
        _   <- ScammanderHelper.dropFirstArg
      } yield {
        val date = LocalDateTime.now().withNano(0).toString
        if (date.startsWith(arg)) Seq(date) else Nil
      }
  }

  implicit val durationParam: Parameter[Duration] = new Parameter[Duration] {

    override val name: String = "duration"

    override def parse[F[_]: Monad: ParserState](source: RootSender, extra: RunExtra)(
        implicit E: ParserError[F]
    ): F[Duration] =
      for {
        arg <- ScammanderHelper.firstArgAndDrop
        res <- {
          val s = arg.content.toUpperCase(Locale.ROOT)

          val usedS = if (!s.contains("T")) {
            val s1 = if (s.contains("D")) {
              if (s.contains("H") || s.contains("M") || s.contains("S")) s.replace("D", "DT")
              else if (s.startsWith("P")) "PT" + s.substring(1)
              else "T" + s
            } else s
            if (!s1.startsWith("P")) "P" + s1 else s1
          } else s

          E.catchNonFatal(Duration.parse(usedS)) { e =>
            Result.syntaxErrorNel(e.getMessage, arg.start)
          }
        }
      } yield res

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      ScammanderHelper.dropFirstArg
  }
}
