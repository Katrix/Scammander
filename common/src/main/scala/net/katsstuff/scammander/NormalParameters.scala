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
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeParseException
import java.util.{Locale, UUID}

import scala.util.Try

trait NormalParameters[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra] =>

  def primitiveParam[A](parName: String, s: String => A): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        if (xs.nonEmpty && xs.head.content.nonEmpty) {
          Try(s(xs.head.content))
            .map(xs.tail -> _)
            .toEither
            .left
            .map(_ => CommandSyntaxError(s"${xs.head.content} is not a valid $name", xs.head.start))
        } else Left(ScammanderHelper.notEnoughArgs)

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
    }

  def mkSingle[A](
      parName: String,
      parser: String => CommandStep[A],
      possibleSuggestions: () => Seq[String]
  ): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        stringParam.parse(source, extra, xs).flatMap(t => parser(t._2).map(res => t._1 -> res))

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): Either[List[RawCmdArg], Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra), _), xs, possibleSuggestions())
    }

  implicit val byteParam:   Parameter[Byte]    = primitiveParam("byte", _.toByte)
  implicit val shortParam:  Parameter[Short]   = primitiveParam("short", _.toShort)
  implicit val intParam:    Parameter[Int]     = primitiveParam("int", _.toInt)
  implicit val longParam:   Parameter[Long]    = primitiveParam("long", _.toLong)
  implicit val floatParam:  Parameter[Float]   = primitiveParam("float", _.toFloat)
  implicit val doubleParam: Parameter[Double]  = primitiveParam("double", _.toDouble)
  implicit val boolParam:   Parameter[Boolean] = primitiveParam("boolean", _.toBoolean)
  implicit val stringParam: Parameter[String]  = primitiveParam("string", identity)
  implicit val unitParam:   Parameter[Unit]    = primitiveParam("", _ => ())

  implicit val urlParam: Parameter[URL] = new Parameter[URL] {
    override def name: String = "url"

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], URL)] = {
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, arg) =>
          Try(new URL(arg))
            .flatMap { url =>
              Try {
                url.toURI //Checks validity
                ys -> url
              }
            }
            .toEither
            .left
            .map(e => CommandSyntaxError(e.getMessage, xs.head.start))
      }
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
  }

  implicit val bigDecimalParam: Parameter[BigDecimal] = primitiveParam("decimal number", BigDecimal.apply)
  implicit val bigIntParam:     Parameter[BigInt]     = primitiveParam("integer number", BigInt.apply)

  implicit val uuidParam: Parameter[UUID] = primitiveParam("uuid", UUID.fromString)

  implicit val dateTimeParam: Parameter[LocalDateTime] = new Parameter[LocalDateTime] {
    override def name: String = "dataTime"
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], LocalDateTime)] = {
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, arg) =>
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
              CommandSyntaxError("Invalid date-time!", xs.head.start)
            }
            .map(ys -> _)
      }
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = {
      val date = LocalDateTime.now.withNano(0).toString
      val arg  = xs.headOption.map(_.content).getOrElse("")

      if (date.startsWith(arg)) Right(Seq(date)) else Left(xs.drop(1))
    }
  }

  implicit val durationParam: Parameter[Duration] = new Parameter[Duration] {
    override def name: String = "duration"
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Duration)] = {
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, arg) =>
          val s = arg.toUpperCase(Locale.ROOT)

          val usedS = if (!s.contains("T")) {
            val s1 = if (s.contains("D")) {
              if (s.contains("H") || s.contains("M") || s.contains("S")) s.replace("D", "DT")
              else if (s.startsWith("P")) "PT" + s.substring(1)
              else "T" + s
            } else s
            if (!s1.startsWith("P")) "P" + s1 else s1
          } else s

          Try(Duration.parse(usedS)).toEither.left
            .map(e => CommandSyntaxError(e.getMessage, xs.head.start))
            .map(ys -> _)
      }
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
  }
}
