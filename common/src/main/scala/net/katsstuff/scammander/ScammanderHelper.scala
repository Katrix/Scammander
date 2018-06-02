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
import java.util.regex.Pattern

import scala.language.higherKinds

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import cats.{Applicative, MonadError}

object ScammanderHelper {

  private val spaceRegex = """\S+""".r
  //https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes
  private val quotedRegex = """(?:"((?:[^"\\]|\\.)+)")|((?:\S)+)""".r

  val notEnoughArgs = CommandSyntaxError("Not enough arguments", -1)

  def notEnoughArgsErrorF[F[_], A](implicit F: MonadError[F, NonEmptyList[CommandFailure]]): F[A] =
    F.raiseError(NonEmptyList.one(notEnoughArgs))

  /**
    * Parse a string argument into [[RawCmdArg]]s which are delimited by whitespace.
    */
  def stringToRawArgs(arguments: String): List[RawCmdArg] =
    spaceRegex.findAllMatchIn(arguments).map(m => RawCmdArg(m.start, m.end, m.matched)).toList

  def dropFirstArg[F[_]](
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], Seq[String]] =
    for {
      _ <- StateT.modifyF[F, List[RawCmdArg]] { xs =>
        if (xs.isEmpty) notEnoughArgsErrorF
        else xs.tail.pure
      }
    } yield Nil

  def getPos[F[_]: Applicative]: StateT[F, List[RawCmdArg], Int] =
    StateT.inspect[F, List[RawCmdArg], Int](_.headOption.fold(-1)(_.start))

  def getArgs[F[_]: Applicative]: StateT[F, List[RawCmdArg], List[RawCmdArg]] =
    StateT.get[F, List[RawCmdArg]]

  def firstArgOpt[F[_]: Applicative]: StateT[F, List[RawCmdArg], Option[RawCmdArg]] =
    StateT.inspect[F, List[RawCmdArg], Option[RawCmdArg]](_.headOption)

  def firstArg[F[_]](implicit F: MonadError[F, NonEmptyList[CommandFailure]]): StateT[F, List[RawCmdArg], RawCmdArg] =
    firstArgOpt[F].flatMapF(_.filter(_.content.nonEmpty).fold[F[RawCmdArg]](notEnoughArgsErrorF)(_.pure))

  def firstArgAndDrop[F[_]](
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], RawCmdArg] = firstArg[F] <* dropFirstArg[F]

  def withFallbackState[F[_], A](first: StateT[F, List[RawCmdArg], A], second: => StateT[F, List[RawCmdArg], A])(
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], A] = {
    StateT[F, List[RawCmdArg], A] { xs =>
      F.handleErrorWith(first.run(xs)) { e1 =>
        F.handleErrorWith(second.run(xs)) { e2 =>
          F.raiseError(e1 ::: e2)
        }
      }
    }
  }

  def fallbackSuggestions[F[_]](
      first: StateT[F, List[RawCmdArg], Seq[String]],
      second: StateT[F, List[RawCmdArg], Seq[String]]
  )(implicit F: MonadError[F, NonEmptyList[CommandFailure]]): StateT[F, List[RawCmdArg], Seq[String]] = {
    StateT[F, List[RawCmdArg], Seq[String]] { xs =>
      F.attemptT(first.run(xs))
        .semiflatMap {
          case (ys, fSuggestions) =>
            F.attemptT(second.run(ys)).getOrElse((ys, fSuggestions))
        }
        .fold(F.raiseError[(List[RawCmdArg], Seq[String])], F.pure)
        .flatten
    }
  }

  def withFallback[F[_], A](first: F[A], second: => F[A])(
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): F[A] =
    F.handleErrorWith(first) { e1 =>
      F.handleErrorWith(second) { e2 =>
        F.raiseError(e1 ::: e2)
      }
    }

  /**
    * Parse a string argument into [[RawCmdArg]]s which are delimited by whitespace
    * as as they are not quoted.
    */
  def stringToRawArgsQuoted(arguments: String): List[RawCmdArg] = {
    if (arguments.isEmpty) List(RawCmdArg(0, 0, ""))
    else {
      val xs = quotedRegex
        .findAllMatchIn(arguments)
        .map { m =>
          val quoted = m.group(1) != null
          val group  = if (quoted) 1 else 2
          RawCmdArg(m.start(group), m.end(group), m.group(group))
        }
        .toList

      if (arguments.endsWith(" ")) xs :+ RawCmdArg(arguments.length - 1, arguments.length - 1, "") else xs
    }
  }

  /**
    * Returns the suggestions for a command given the argument list and
    * all the possible string suggestions.
    */
  def suggestions[F[_], E](parse: StateT[F, List[RawCmdArg], E], choices: => Iterable[String])(
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], Seq[String]] = {
    for {
      xs <- getArgs[F]
      parsed <- StateT.liftF {
        //There is no point in getting suggestions if there are no args
        if (xs == Nil) notEnoughArgsErrorF[F, Either[NonEmptyList[CommandFailure], (List[RawCmdArg], E)]]
        else F.attempt(parse.run(xs))
      }
      _ <- StateT.set(parsed.map(_._1).getOrElse(Nil))
    } yield {
      val content = xs.head.content

      if (content.isEmpty) choices.toSeq
      else {
        val startsWith = choices.filter(_.startsWith(content)).toSeq
        if (startsWith.lengthCompare(1) == 0 && choices.exists(_.equalsIgnoreCase(content)))
          Nil
        else
          startsWith
      }
    }
  }

  /**
    * Returns the suggestions for a command given the argument list and
    * all the possible suggestions.
    */
  def suggestionsNamed[F[_], A, E](parse: StateT[F, List[RawCmdArg], E], choices: => Iterable[A])(
      implicit named: HasName[A],
      F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], Seq[String]] = suggestions(parse, choices.map(named.apply))

  /**
    * Parse a single paramter given the current argument list, and a map of the valid choices.
    */
  def parse[F[_], A](name: String, choices: Map[String, A])(
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], A] = {
    require(choices.keys.forall(s => s.toLowerCase(Locale.ROOT) == s))

    def usageError(arg: RawCmdArg) =
      F.raiseError(NonEmptyList.one(CommandUsageError(s"${arg.content} is not a valid $name", arg.start)))

    for {
      arg <- firstArgAndDrop
      optValue = choices.get(arg.content.toLowerCase(Locale.ROOT))
      res <- StateT.liftF(optValue.fold[F[A]](usageError(arg)(F.pure(_))))
    } yield res
  }

  /**
    * Parse a paramter given the current argument list, and a list of the valid choices.
    */
  def parse[F[_], A](name: String, choices: Iterable[A])(
      implicit named: HasName[A],
      F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], A] = parse(name, choices.map(obj => named(obj) -> obj).toMap)

  /**
    * Parse a set for a paramter given the current argument list, and a map of the valid choices.
    */
  //Based on PatternMatchingCommandElement in Sponge
  def parseMany[F[_], A](name: String, choices: Map[String, A])(
      implicit F: MonadError[F, NonEmptyList[CommandFailure]]
  ): StateT[F, List[RawCmdArg], Set[A]] = {
    def formattedPattern(input: String) = {
      // Anchor matches to the beginning -- this lets us use find()
      val usedInput = if (!input.startsWith("^")) s"^$input" else input
      Pattern.compile(usedInput, Pattern.CASE_INSENSITIVE)
    }

    for {
      arg <- firstArgAndDrop
      res <- StateT.liftF {
        val RawCmdArg(pos, _, unformattedPattern) = arg

        val pattern         = formattedPattern(unformattedPattern)
        val filteredChoices = choices.filterKeys(k => pattern.matcher(k).find())
        filteredChoices
          .collectFirst {
            case (k, v) if k.equalsIgnoreCase(unformattedPattern) => F.pure(Set(v))
          }
          .getOrElse {
            if (filteredChoices.nonEmpty) {
              F.pure(filteredChoices.values.toSet)
            } else {
              F.raiseError(NonEmptyList.one(CommandUsageError(s"$unformattedPattern is not a valid $name", pos)))
            }
          }
      }
    } yield res
  }

  /**
    * Parse a set for a paramter given the current argument list, and a list of the valid choices.
    */
  def parseMany[F[_], A](
      name: String,
      choices: Iterable[A]
  )(implicit named: HasName[A], F: MonadError[F, NonEmptyList[CommandFailure]]): StateT[F, List[RawCmdArg], Set[A]] =
    parseMany(name, choices.map(obj => named(obj).toLowerCase(Locale.ROOT) -> obj).toMap)
}
