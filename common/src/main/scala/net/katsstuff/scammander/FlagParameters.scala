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

import java.util.Locale

import scala.language.higherKinds

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
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
  ): Parameter[ValueFlag[Name, A]] = new Parameter[ValueFlag[Name, A]] {
    private val flagName      = if (witness.value.size > 1) s"--${witness.value}" else s"-${witness.value}"
    override def name: String = s"$flagName ${flagParam.name}"

    override def parse(source: RootSender, extra: RunExtra): SF[ValueFlag[Name, A]] =
      ScammanderHelper.getArgs[F].flatMap { xs =>
        val matchingIndices = xs.zipWithIndex.collect {
          case (RawCmdArg(_, _, content), idx) if content.equalsIgnoreCase(flagName) => idx
        }

        NonEmptyList
          .fromList(matchingIndices)
          .fold[SF[ValueFlag[Name, A]]](SF.pure(ValueFlag(None))) {
            case NonEmptyList(singleIdx, Nil) =>
              val before = xs.take(singleIdx)
              flagParam
                .parse(source, extra)
                .contramap[List[RawCmdArg]](_.drop(singleIdx + 1))
                .modify(ys => before ::: ys)
                .map(a => ValueFlag(Some(a)))
            case more =>
              StateT.liftF(F.raiseError(more.map(idx => Command.usageError(s"$flagName is already defined", idx))))
          }
      }

    override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
      ScammanderHelper.getArgs[F].flatMap { xs =>
        val matchingIndices = xs.zipWithIndex.collect {
          case (RawCmdArg(_, _, content), idx)
              if content.nonEmpty && flagName.startsWith(content.toLowerCase(Locale.ROOT)) =>
            (content.equalsIgnoreCase(flagName), idx)
        }

        NonEmptyList
          .fromList(matchingIndices)
          .fold(SF.pure(Nil: Seq[String])) {
            case NonEmptyList((false, _), Nil) =>
              SF.pure(Seq(flagName))
            case NonEmptyList((true, singleIdx), Nil) =>
              val before = xs.take(singleIdx)
              println(before)
              flagParam
                .suggestions(source, extra)
                .contramap[List[RawCmdArg]] { ys =>
                  println(ys)
                  ys.drop(singleIdx + 1)
                }
                .modify(ys => before ::: ys)
            case more =>
              StateT.liftF(F.raiseError(more.map(idx => Command.usageError(s"$flagName is already defined", idx._2))))
          }
      }

    override def usage(source: RootSender): F[String] = flagParam.usage(source).map(fUsage => s"$flagName $fUsage")
  }

  /**
    * Parses a flag.
    * @param present If the flag was present.
    * @tparam Name The name of the flag.
    */
  case class BooleanFlag[Name <: String](present: Boolean)
  implicit def booleanFlagParameter[Name <: String](implicit witness: Witness.Aux[Name]): Parameter[BooleanFlag[Name]] =
    new Parameter[BooleanFlag[Name]] {
      private val flagName =
        if (witness.value.size > 1) s"--${witness.value}".toLowerCase(Locale.ROOT)
        else s"-${witness.value}".toLowerCase(Locale.ROOT)

      override val name: String = flagName

      override def parse(source: RootSender, extra: RunExtra): SF[BooleanFlag[Name]] =
        ScammanderHelper.getArgs[F].transformF { fa =>
          fa.flatMap {
            case (xs, _) =>
              val matchingIndices = xs.zipWithIndex.collect {
                case (RawCmdArg(_, _, content), idx) if content.equalsIgnoreCase(flagName) => idx
              }

              NonEmptyList.fromList(matchingIndices).fold(F.pure((xs, BooleanFlag[Name](present = false)))) {
                case NonEmptyList(singleIdx, Nil) => F.pure((xs.patch(singleIdx, Nil, 1), BooleanFlag(true)))
                case more                         => F.raiseError(more.map(idx => Command.usageError(s"$flagName is already defined", idx)))
              }
          }
        }

      override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
        ScammanderHelper.getArgs[F].transformF { fa =>
          fa.flatMap {
            case (xs, _) =>
              val matchingIndices = xs.zipWithIndex.collect {
                case (RawCmdArg(_, _, content), idx)
                    if content.nonEmpty && flagName.startsWith(content.toLowerCase(Locale.ROOT)) =>
                  (content.equalsIgnoreCase(flagName), idx)
              }

              NonEmptyList.fromList(matchingIndices).fold(F.pure((xs, Nil: Seq[String]))) {
                case NonEmptyList((false, singleIdx), Nil) => F.pure((xs, Seq(flagName)))
                case NonEmptyList((true, singleIdx), Nil)  => F.pure((xs.patch(singleIdx, Nil, 1), Nil))
                case more                                  => F.raiseError(more.map(idx => Command.usageError(s"$flagName is already defined", idx._2)))
              }
          }
        }

      override def usage(source: RootSender): F[String] = flagName.pure
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

    override val name: String = s"${paramParam.name} ${flagsParam.name}"

    override def parse(source: RootSender, extra: RunExtra): SF[Flags[A, B]] =
      for {
        t1 <- flagsParam.parse(source, extra)
        t2 <- paramParam.parse(source, extra)
      } yield Flags(t1, t2)

    override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
      for {
        flagSuggestions  <- flagsParam.suggestions(source, extra)
        paramSuggestions <- paramParam.suggestions(source, extra)
      } yield flagSuggestions ++ paramSuggestions
  }
}
