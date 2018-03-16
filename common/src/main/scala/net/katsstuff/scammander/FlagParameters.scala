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

import scala.annotation.tailrec

import shapeless.Witness

import net.katsstuff.scammander.CrossCompatibility._

trait FlagParameters[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra] =>

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

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], ValueFlag[Name, A])] = {

        @tailrec
        def inner(ys: List[RawCmdArg], acc: List[RawCmdArg]): CommandStep[(List[RawCmdArg], ValueFlag[Name, A])] = {
          if (ys.nonEmpty) {
            val h = ys.head
            if (h.content == flagName) {
              flagParam.parse(source, extra, ys.tail).map(t => (acc reverse_::: t._1, ValueFlag(Some(t._2))))
            } else inner(ys.tail, h :: acc)
          } else Right((acc.reverse, ValueFlag(None)))
        }

        inner(xs, Nil)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): Either[List[RawCmdArg], Seq[String]] = {

        val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = args => {
          val res = args.headOption.exists(arg => flagName.equalsIgnoreCase(arg.content))
          if (res) Right(xs.tail -> true) else Left(Command.error("Not a flag"))
        }

        @tailrec
        def inner(xs: List[RawCmdArg], acc: List[RawCmdArg]): Either[List[RawCmdArg], Seq[String]] = {
          if (xs.isEmpty) Left(acc.reverse)
          else if (xs.head.content.startsWith("-")) {
            ScammanderHelper.suggestions(parse, xs, Seq(flagName)) match {
              case Right(suggestions) => Right(suggestions)
              case Left(_) if xs.headOption.map(_.content).exists(_.equalsIgnoreCase(flagName)) =>
                flagParam.suggestions(source, extra, xs.tail)
              case Left(_) => inner(xs.tail, xs.head :: acc)
            }
          } else Left(xs)
        }

        if (xs.isEmpty || xs.head.content.isEmpty) Right(Seq(flagName)) else inner(xs, Nil)
      }

      override def usage(source: RootSender): String = s"$flagName ${flagParam.usage(source)}"
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

      override def parse(
          source: RootSender,
          extra: RunExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], BooleanFlag[Name])] = {

        @tailrec
        def inner(ys: List[RawCmdArg], acc: List[RawCmdArg]): CommandStep[(List[RawCmdArg], BooleanFlag[Name])] = {
          if (ys.nonEmpty) {
            val h = ys.head
            if (h.content == flagName) {
              Right((acc reverse_::: ys.tail, BooleanFlag(true)))
            } else inner(ys.tail, h :: acc)
          } else Right((acc.reverse, BooleanFlag(false)))
        }

        inner(xs, Nil)
      }

      override def suggestions(
          source: RootSender,
          extra: TabExtra,
          xs: List[RawCmdArg]
      ): Either[List[RawCmdArg], Seq[String]] = {
        val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = args => {
          val res = args.headOption.exists(arg => flagName.equalsIgnoreCase(arg.content))
          if (res) Right(xs.tail -> true) else Left(Command.error("Not a flag"))
        }

        @tailrec
        def inner(xs: List[RawCmdArg], acc: List[RawCmdArg]): Either[List[RawCmdArg], Seq[String]] = {
          if (xs.isEmpty) Left(acc.reverse)
          else if (xs.head.content.startsWith("-")) {
            ScammanderHelper.suggestions(parse, xs, Seq(flagName)) match {
              case Right(suggestions) => Right(suggestions)
              case Left(_)            => inner(xs.tail, xs.head :: acc)
            }
          } else Left(xs)
        }

        if (xs.isEmpty || xs.head.content.isEmpty) Right(Seq(flagName)) else inner(xs, Nil)
      }

      override def usage(source: RootSender): String = flagName
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

    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Flags[A, B])] = {
      for {
        t1 <- flagsParam.parse(source, extra, xs)
        t2 <- paramParam.parse(source, extra, t1._1)
      } yield t2._1 -> Flags(t1._2, t2._2)
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = {
      val flagSuggestions  = flagsParam.suggestions(source, extra, xs)

      flagSuggestions match {
        case Left(ys)           => paramParam.suggestions(source, extra, ys)
        case Right(suggestions) => Right(suggestions ++ paramParam.suggestions(source, extra, xs).getOrElse(Nil))
      }
    }
  }
}
