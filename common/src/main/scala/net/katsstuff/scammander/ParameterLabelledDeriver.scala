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

import shapeless._
import shapeless.labelled.FieldType

trait ParameterLabelledDeriver[RootSender, RunExtra, TabExtra]
    extends ParameterDeriver[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra] =>

  object autoderivation {
    implicit def autoGenParam[A, Gen](
        implicit gen: LabelledGeneric.Aux[A, Gen],
        param: Lazy[Parameter[Gen]]
    ): Parameter[A] = genParam[A, Gen]
  }

  class ParameterDeriver[A] {
    def derive[Repr](implicit gen: LabelledGeneric.Aux[A, Repr], param: Parameter[Repr]): Parameter[A] =
      genParam[A, gen.Repr]
  }
  object ParameterDeriver {
    def apply[A]: ParameterDeriver[A] = new ParameterDeriver[A]
  }

  def genParam[A, Gen](implicit gen: LabelledGeneric.Aux[A, Gen], genParam: Lazy[Parameter[Gen]]): Parameter[A] =
    new ProxyParameter[A, Gen] {
      override def param: Parameter[Gen] = genParam.value

      override def parse(source: RootSender, extra: RunExtra, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
        genParam.value.parse(source, extra, xs).map(t => t._1 -> gen.from(t._2))
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
        } yield (t2._1, labelled.field[HK](t1._2) :: t2._2)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): Either[List[RawCmdArg], Seq[String]] = {
        for {
          ys <- hParam.value.suggestions(source, extra, xs).left
          zs <- tParam.value.suggestions(source, extra, ys).left
        } yield zs
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hName.value.name
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) s"<$hUsage>" else s"<$hUsage> $tUsage"
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
      ): Either[List[RawCmdArg], Seq[String]] = {
        val eh = hParam.value.suggestions(source, extra, xs)
        val et = tParam.value.suggestions(source, extra, xs)
        if (eh.isRight || et.isRight) Right(eh.getOrElse(Nil) ++ et.getOrElse(Nil))
        else {
          val rest = if (eh.left.get.lengthCompare(et.left.get.size) > 0) eh.left.get else et.left.get
          Left(rest)
        }
      }

      override def usage(source: RootSender): String = {
        lazy val hUsage = hParam.value.usage(source)
        lazy val tUsage = tParam.value.usage(source)

        if (tUsage.isEmpty) s"($hUsage)" else s"($hUsage)|$tUsage"
      }
    }
}
