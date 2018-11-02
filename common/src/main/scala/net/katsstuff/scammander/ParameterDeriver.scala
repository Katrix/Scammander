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

import cats.syntax.all._
import shapeless._

trait ParameterDeriver { self: ScammanderBase =>

  implicit def hConsParam[H, T <: HList](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :: T] = new Parameter[H :: T] {
    override def name: String = s"${hParam.value.name} ${tParam.value.name}"

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[H :: T] =
      for {
        h <- hParam.value.parse(source, extra)
        t <- tParam.value.parse(source, extra)
      } yield h :: t

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      ScammanderHelper.withFallbackSuggestions(
        hParam.value.suggestions(source, extra),
        tParam.value.suggestions(source, extra)
      )

    override def usage[F[_]: ParserError](source: RootSender): F[String] = {
      val hUsage = hParam.value.usage(source)
      val tUsage = tParam.value.usage(source)

      hUsage.map2(tUsage) { (h, t) =>
        if (t.isEmpty) h else s"$h $t"
      }
    }
  }

  implicit val hNilParam: Parameter[HNil] = new Parameter[HNil] {
    override def name: String = ""

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[HNil] =
      (HNil: HNil).pure

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      Result.errorF("Suggestions on HNil")

    override def usage[F[_]: ParserError](source: RootSender): F[String] = "".pure
  }

  implicit def cConsParam[H, T <: Coproduct](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :+: T] =
    new Parameter[H :+: T] {
      override def name: String = s"${hParam.value.name}|${tParam.value.name}"

      override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[H :+: T] = {
        val hParse: F[H :+: T]      = hParam.value.parse(source, extra).map(Inl.apply)
        lazy val tParse: F[H :+: T] = tParam.value.parse(source, extra).map(Inr.apply)

        ScammanderHelper.withFallback(hParse, tParse)
      }

      override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] = {
        val sfh = hParam.value.suggestions(source, extra)
        val sft = tParam.value.suggestions(source, extra)

        sfh.map2(sft)(_ ++ _)
      }

      override def usage[F[_]: ParserError](source: RootSender): F[String] = {
        val hUsage = hParam.value.usage(source)
        val tUsage = tParam.value.usage(source)

        hUsage.map2(tUsage) { (h, t) =>
          if (t.isEmpty) h else s"$h|$t"
        }
      }
    }

  implicit val cNilParam: Parameter[CNil] = new Parameter[CNil] {
    override def name: String = ""

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[CNil] =
      ScammanderHelper.getPos.flatMap(pos => Result.syntaxErrorF("Could not parse argument", pos))

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      (Nil: Seq[String]).pure

    override def usage[F[_]: ParserError](source: RootSender): F[String] = "".pure
  }
}
