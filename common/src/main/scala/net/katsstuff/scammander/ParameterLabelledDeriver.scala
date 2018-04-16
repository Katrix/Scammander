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
import cats.data.StateT
import shapeless._
import shapeless.labelled.FieldType

trait ParameterLabelledDeriver[F[_], RootSender, RunExtra, TabExtra]
    extends ParameterDeriver[F, RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra] =>

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
    new ProxyParameter[A, Gen] {
      override def param: Parameter[Gen] = genParam.value

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], A] =
        genParam.value.parse(source, extra).map(gen.from)
    }

  implicit def hConsLabelledParam[HK <: Symbol, HV, T <: HList](
      implicit
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :: T] =
    new Parameter[FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name} ${tParam.value.name}"

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], FieldType[HK, HV] :: T] =
        for {
          h <- hParam.value.parse(source, extra)
          t <- tParam.value.parse(source, extra)
        } yield labelled.field[HK](h) :: t

      override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Option[Seq[String]]] = {
        hParam.value.suggestions(source, extra).flatMap {
          case Some(ret) => StateT.pure(Some(ret))
          case None      => tParam.value.suggestions(source, extra)
        }
      }

      override def usage(source: RootSender): F[String] = {
        lazy val hUsage = hName.value.name
        lazy val tUsage = tParam.value.usage(source)

        F.map(tUsage)(t => if (t.isEmpty) s"<$hUsage>" else s"<$hUsage> $t")
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

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], FieldType[HK, HV] :+: T] = {
        val hParse: StateT[F, List[RawCmdArg], FieldType[HK, HV] :+: T] =
          hParam.value.parse(source, extra).map(h => Inl(labelled.field[HK](h)))
        lazy val tParse: StateT[F, List[RawCmdArg], FieldType[HK, HV] :+: T] =
          tParam.value.parse(source, extra).map(Inr.apply)

        ScammanderHelper.withFallback(hParse, tParse)
      }

      override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Option[Seq[String]]] = {
        val eh = hParam.value.suggestions(source, extra)
        val et = tParam.value.suggestions(source, extra)

        val SF = Monad[StateT[F, List[RawCmdArg], ?]]

        SF.map2(eh, et) {
          case (Some(h), Some(t)) => Some(h ++ t)
          case (Some(h), None)    => Some(h)
          case (None, Some(t))    => Some(t)
          case (None, None)       => None
        }
      }

      override def usage(source: RootSender): F[String] = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        F.map2(hUsage, tUsage) { (h, t) =>
          if (t.isEmpty) s"($h)" else s"($h)|$t"
        }
      }
    }
}
