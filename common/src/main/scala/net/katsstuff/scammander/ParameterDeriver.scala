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

import cats.data.StateT
import cats.syntax.all._
import shapeless._

trait ParameterDeriver[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra] =>

  implicit def hConsParam[H, T <: HList](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :: T] =
    new Parameter[H :: T] {
      override def name: String = s"${hParam.value.name} ${tParam.value.name}"

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], H :: T] =
        for {
          h <- hParam.value.parse(source, extra)
          t <- tParam.value.parse(source, extra)
        } yield h :: t

      override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
        ScammanderHelper.fallbackSuggestions(
          hParam.value.suggestions(source, extra),
          tParam.value.suggestions(source, extra)
        )

      override def usage(source: RootSender): F[String] = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        F.map2(hUsage, tUsage) { (h, t) =>
          if (t.isEmpty) h else s"$h $t"
        }
      }
    }

  implicit val hNilParam: Parameter[HNil] = new Parameter[HNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], HNil] =
      SF.pure(HNil)

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
      SF.pure(Nil)

    override def usage(source: RootSender): F[String] = F.pure("")
  }

  implicit def cConsParam[H, T <: Coproduct](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :+: T] =
    new Parameter[H :+: T] {
      override def name: String = s"${hParam.value.name}|${tParam.value.name}"

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], H :+: T] = {
        val hParse:      StateT[F, List[RawCmdArg], H :+: T] = hParam.value.parse(source, extra).map(Inl.apply)
        lazy val tParse: StateT[F, List[RawCmdArg], H :+: T] = tParam.value.parse(source, extra).map(Inr.apply)

        ScammanderHelper.withFallbackState(hParse, tParse)
      }

      override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] = {
        val sfh = hParam.value.suggestions(source, extra)
        val sft = tParam.value.suggestions(source, extra)

        SF.map2(sfh, sft)(_ ++ _)
      }

      override def usage(source: RootSender): F[String] = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        F.map2(hUsage, tUsage) { (h, t) =>
          if (t.isEmpty) h else s"$h|$t"
        }
      }
    }

  implicit val cNilParam: Parameter[CNil] = new Parameter[CNil] {
    override def name: String = ""

    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], CNil] =
      ScammanderHelper.getPos[F].flatMapF(pos => Command.syntaxErrorF("Could not parse argument", pos))

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
      SF.pure(Nil)

    override def usage(source: RootSender): F[String] = "".pure
  }
}
