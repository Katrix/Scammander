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

import cats.Monad
import cats.effect.Async
import cats.syntax.all._
import shapeless._
import shapeless.labelled.FieldType

trait ParameterLabelledDeriver extends ParameterDeriver { self: ScammanderBase =>

  object autoderivation {
    implicit def autoGenParam[A, Gen](
        implicit gen: LabelledGeneric.Aux[A, Gen],
        param: Lazy[Parameter[Gen]]
    ): Parameter[A] = genParam[A, Gen]
  }

  class ParameterDeriver[A] {
    def derive[Repr](implicit gen: LabelledGeneric.Aux[A, Repr], param: Lazy[Parameter[Repr]]): Parameter[A] =
      genParam[A, gen.Repr]
  }
  object ParameterDeriver {
    def apply[A]: ParameterDeriver[A] = new ParameterDeriver[A]
  }

  def genParam[A, Gen](implicit gen: LabelledGeneric.Aux[A, Gen], genParam: Lazy[Parameter[Gen]]): Parameter[A] =
    genParam.value.map(gen.from)

  implicit def hConsLabelledParam[HK <: Symbol, HV, T <: HList](
      implicit
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :: T] =
    new Parameter[FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name} ${tParam.value.name}"

      override def parse[F[_]: Monad: ParserState: ParserError](
          source: RootSender,
          extra: RunExtra
      ): F[FieldType[HK, HV] :: T] =
        for {
          h <- hParam.value.parse(source, extra)
          t <- tParam.value.parse(source, extra)
        } yield labelled.field[HK](h) :: t

      override def suggestions[F[_]: Async: ParserState: ParserError](
          source: RootSender,
          extra: TabExtra
      ): F[Seq[String]] =
        ScammanderHelper.withFallbackSuggestions(
          hParam.value.suggestions(source, extra),
          tParam.value.suggestions(source, extra)
        )

      override def usage[F[_]: Monad: ParserError](source: RootSender): F[String] = {
        val hUsage = hName.value.name
        val tUsage = tParam.value.usage(source)
        tUsage.map(t => if (t.isEmpty) s"<$hUsage>" else s"<$hUsage> $t")
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

      override def parse[F[_]: Monad: ParserState: ParserError](
          source: RootSender,
          extra: RunExtra
      ): F[FieldType[HK, HV] :+: T] = {
        val hParse: F[FieldType[HK, HV] :+: T] =
          hParam.value.parse(source, extra).map(h => Inl(labelled.field[HK](h)))
        lazy val tParse: F[FieldType[HK, HV] :+: T] = tParam.value.parse(source, extra).map(Inr.apply)

        ScammanderHelper.withFallback(hParse, tParse)
      }

      override def suggestions[F[_]: Async: ParserState: ParserError](
          source: RootSender,
          extra: TabExtra
      ): F[Seq[String]] = {
        val sfh = hParam.value.suggestions(source, extra)
        val sft = tParam.value.suggestions(source, extra)

        sfh.map2(sft)(_ ++ _)
      }

      override def usage[F[_]: Monad: ParserError](source: RootSender): F[String] = {
        val hUsage = hParam.value.usage(source)
        val tUsage = tParam.value.usage(source)

        hUsage.map2(tUsage) { (h, t) =>
          if (t.isEmpty) s"($h)" else s"($h)|$t"
        }
      }
    }
}
