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
import shapeless.Witness

trait FlagParameters[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra] =>

  /**
    * Parses a flag with a value followed by it.
    * @param value The value if it was present.
    * @tparam Name The name of the flag.
    * @tparam A The type of the value.
    */
  case class ValueFlag[Name <: String, A](value: Option[A])
  implicit def valueFlagParameter[Name <: String, A](
      implicit witness: Witness.Aux[Name],
      flagParam: Parameter[A]
  ): Parameter[ValueFlag[Name, A]] =
    new Parameter[ValueFlag[Name, A]] {
      private val flagName = if (witness.value.size > 1) s"--${witness.value}" else s"-${witness.value}"
      override def name: String = s"$flagName ${flagParam.name}"

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], ValueFlag[Name, A]] = {

        ScammanderHelper.getArgs[F].flatMap { xs =>
          val idx = xs.indexWhere(_.content.equalsIgnoreCase(flagName))
          if (idx == -1) StateT.pure(ValueFlag(None))
          else {
            val before = xs.take(idx)
            flagParam
              .parse(source, extra)
              .contramap[List[RawCmdArg]](_.drop(idx + 1))
              .modify(ys => before ::: ys)
              .map(a => ValueFlag(Some(a)))
          }
        }
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
      ): StateT[F, List[RawCmdArg], Option[Seq[String]]] = {

        val parse1 = ScammanderHelper
          .getArgs[F]
          .flatMapF[Boolean] { xs =>
            val idx = xs.indexWhere(_.content.equalsIgnoreCase(flagName))
            if (idx != -1) F.pure(true)
            else Command.errorF("Not a flag")
          }
          .modify(_.tail)

        ScammanderHelper.getArgs[F].flatMap { xs =>
          if (xs.isEmpty || xs.head.content.isEmpty) StateT.pure(Some(Seq(flagName)))
          else {
            val idx = xs.indexWhere(_.content.startsWith("-"))
            if (idx == -1) StateT.pure(None)
            else {
              val before = xs.take(idx)
              ScammanderHelper
                .suggestions(parse1, Seq(flagName))
                .contramap[List[RawCmdArg]](_.drop(idx + 1))
                .modify(ys => before ::: ys)
                .flatMap(
                  suggestions => suggestions.fold(flagParam.suggestions(source, extra))(seq => StateT.pure(Some(seq)))
                )
            }
          }
        }
      }

      override def usage(source: RootSender): F[String] = F.map(flagParam.usage(source))(fUsage => s"$flagName $fUsage")
    }

  /**
    * Parses a flag.
    * @param present If the flag was present.
    * @tparam Name The name of the flag.
    */
  case class BooleanFlag[Name <: String](present: Boolean)
  implicit def booleanFlagParameter[Name <: String](implicit witness: Witness.Aux[Name]): Parameter[BooleanFlag[Name]] =
    new Parameter[BooleanFlag[Name]] {
      private val flagName = if (witness.value.size > 1) s"--${witness.value}" else s"-${witness.value}"
      override def name: String = flagName

      override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], BooleanFlag[Name]] = {

        ScammanderHelper.getArgs[F].transform { (xs, _) =>
          val idx = xs.indexWhere(_.content.equalsIgnoreCase(flagName))
          if (idx == -1) (xs, BooleanFlag(false))
          else (xs.patch(idx, Nil, 1), BooleanFlag(true))
        }
      }

      override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Option[Seq[String]]] = {
        val parse = ScammanderHelper
          .getArgs[F]
          .flatMapF[Boolean] { xs =>
            val idx = xs.indexWhere(_.content.equalsIgnoreCase(flagName))
            if (idx != -1) F.pure(true)
            else Command.errorF("Not a flag")
          }
          .modify(_.tail)

        ScammanderHelper.getArgs[F].flatMap { xs =>
          if (xs.isEmpty || xs.head.content.isEmpty) StateT.pure(Some(Seq(flagName)))
          else {
            val idx = xs.indexWhere(_.content.startsWith("-"))
            if (idx == -1) StateT.pure(None)
            else {
              val before = xs.take(idx)
              ScammanderHelper
                .suggestions(parse, Seq(flagName))
                .contramap[List[RawCmdArg]](_.drop(idx + 1))
                .modify(ys => before ::: ys)
            }
          }
        }
      }

      override def usage(source: RootSender): F[String] = F.pure(flagName)
    }

  /**
    * A helper to group flags together with normal parameters.
    * @param flags The flags values.
    * @param parameters The parameter values.
    * @tparam A The flag parameters.
    * @tparam B The other value parameters.
    */
  case class Flags[A, B](flags: A, parameters: B)
  implicit def flagsParameter[A, B](
      implicit flagsParam: Parameter[A],
      paramParam: Parameter[B]
  ): Parameter[Flags[A, B]] = new Parameter[Flags[A, B]] {
    override def name: String = s"${paramParam.name} ${flagsParam.name}"

    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], Flags[A, B]] = {
      for {
        t1 <- flagsParam.parse(source, extra)
        t2 <- paramParam.parse(source, extra)
      } yield Flags(t1, t2)
    }

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Option[Seq[String]]] = {
      for {
        flagSuggestions  <- flagsParam.suggestions(source, extra)
        paramSuggestions <- paramParam.suggestions(source, extra)
      } yield
        (flagSuggestions, paramSuggestions) match {
          case (Some(flag), Some(param)) => Some(flag ++ param)
          case (Some(flag), None)        => Some(flag)
          case (None, Some(param))       => Some(param)
          case (None, None)              => None
        }
    }
  }
}
