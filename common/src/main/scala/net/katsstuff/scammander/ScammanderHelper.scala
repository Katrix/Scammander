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

import java.util.Locale
import java.util.regex.Pattern

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Eval, Monad}
import net.katsstuff.scammander.ScammanderTypes._

object ScammanderHelper {

  implicit private[scammander] class StringOps(private val string: String) extends AnyVal {
    def toLowercaseRoot: String = string.toLowerCase(Locale.ROOT)
  }

  private val spaceRegex = """\S+""".r
  //https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes
  private val quotedRegex = """(?:"((?:[^"\\]|\\.)+)")|((?:\S)+)""".r

  val notEnoughArgs: CommandFailure = Result.syntaxError("Not enough arguments", -1)

  val notEnoughArgsNel: NonEmptyList[CommandFailure] = NonEmptyList.one(notEnoughArgs)

  def notEnoughArgsErrorF[F[_], A](implicit E: ParserError[F]): F[A] =
    E.raise(notEnoughArgsNel)

  /**
    * Parse a string argument into [[RawCmdArg]]s which are delimited by whitespace.
    */
  def stringToRawArgs(arguments: String): List[RawCmdArg] =
    spaceRegex.findAllMatchIn(arguments).map(m => RawCmdArg(m.start, m.end, m.matched)).toList

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

      val lastPos = arguments.length - 1
      if (arguments.endsWith(" ")) xs :+ RawCmdArg(lastPos, lastPos, "") else xs
    }
  }

  def dropFirstArg[F[_]: Monad](implicit E: ParserError[F], S: ParserState[F]): F[Seq[String]] =
    for {
      xs <- S.get
      _  <- if (xs.isEmpty) notEnoughArgsErrorF else S.set(xs.tail)
    } yield Nil

  def getPos[F[_]](implicit S: ParserState[F]): F[Int] = S.inspect(_.headOption.fold(-1)(_.start))

  def firstArgOpt[F[_]](implicit S: ParserState[F]): F[Option[RawCmdArg]] = S.inspect(_.headOption)

  def firstArg[F[_]: Monad: ParserState](implicit E: ParserError[F]): F[RawCmdArg] =
    firstArgOpt.map(_.filter(_.content.nonEmpty).toRight(notEnoughArgsNel)).flatMap(_.fold(E.raise, _.pure))

  def firstArgAndDrop[F[_]: Monad: ParserState: ParserError]: F[RawCmdArg] = firstArg <* dropFirstArg

  def withFallbackSuggestions[F[_]: Monad](first: F[Seq[String]], second: F[Seq[String]])(
      implicit E: ParserError[F]
  ): F[Seq[String]] =
    for {
      fSuggestions <- first
      res          <- E.attempt(second).map(_.getOrElse(fSuggestions))
    } yield res

  def withFallback[F[_], A](first: F[A], second: F[A])(implicit E: ParserError[F]): F[A] =
    E.handleWith(first) { e1 =>
      E.handleWith(second) { e2 =>
        E.raise(e1 ::: e2)
      }
    }

  /**
    * Returns the suggestions for a command given the argument list and
    * all the possible string suggestions.
    */
  def suggestions[F[_]: Monad, E](parse: F[E], choices: Eval[Iterable[String]])(
      implicit E: ParserError[F],
      S: ParserState[F]
  ): F[Seq[String]] =
    for {
      xs <- S.get
      //There is no point in getting suggestions if there are no args
      parsed <- if (xs == Nil) notEnoughArgsErrorF[F, Either[CommandFailureNEL, E]] else E.attempt(parse)
      _      <- if (parsed.isLeft) S.set(Nil) else ().pure
    } yield {
      val content = xs.head.content

      val evaluatedChoices = choices.value
      if (content.isEmpty) evaluatedChoices.toSeq
      else {
        val startsWith = evaluatedChoices.filter(_.startsWith(content)).toSeq
        if (startsWith.lengthCompare(1) == 0 && evaluatedChoices.exists(_.equalsIgnoreCase(content)))
          Nil
        else
          startsWith
      }
    }

  /**
    * Returns the suggestions for a command given the argument list and
    * all the possible suggestions.
    */
  def suggestionsNamed[F[_]: Monad: ParserError: ParserState, A, E](parse: F[E], choices: Eval[Iterable[A]])(
      implicit named: HasName[A],
  ): F[Seq[String]] = suggestions(parse, choices.map(_.map((named.apply _).andThen(_.toLowercaseRoot))))

  /**
    * Parse a single paramter given the current argument list, and a map of the valid choices.
    */
  def parse[F[_]: Monad: ParserState, A](name: String, choices: Map[String, A])(
      implicit E: ParserError[F]
  ): F[A] = {
    def usageError(arg: RawCmdArg) =
      NonEmptyList.one(Result.usageError(s"${arg.content} is not a valid $name", arg.start))

    for {
      arg <- firstArgAndDrop
      optValue = choices
        .filterKeys(_.equalsIgnoreCase(arg.content))
        .headOption
        .map(_._2)
      res <- optValue.fold(E.raise[A](usageError(arg)))(_.pure)
    } yield res
  }

  /**
    * Parse a paramter given the current argument list, and a list of the valid choices.
    */
  def parse[F[_]: Monad: ParserState: ParserError, A](name: String, choices: Iterable[A])(
      implicit named: HasName[A],
  ): F[A] = parse(name, choices.map(obj => named(obj).toLowercaseRoot -> obj).toMap)

  /**
    * Parse a set for a paramter given the current argument list, and a map of the valid choices.
    */
  //Based on PatternMatchingCommandElement in Sponge
  def parseMany[F[_]: Monad, A](name: String, choices: Map[String, A])(
      implicit E: ParserError[F],
      S: ParserState[F]
  ): F[Set[A]] = {
    def formattedPattern(input: String) = {
      // Anchor matches to the beginning -- this lets us use find()
      val usedInput = if (!input.startsWith("^")) s"^$input" else input
      Pattern.compile(usedInput, Pattern.CASE_INSENSITIVE)
    }

    for {
      arg <- firstArgAndDrop
      RawCmdArg(pos, _, unformattedPattern) = arg

      pattern         = formattedPattern(unformattedPattern)
      filteredChoices = choices.filterKeys(k => pattern.matcher(k).find())
      res <- filteredChoices
        .collectFirst {
          case (k, v) if k.equalsIgnoreCase(unformattedPattern) => Set(v).pure
        }
        .getOrElse {
          if (filteredChoices.nonEmpty)
            filteredChoices.values.toSet.pure
          else
            E.raise(NonEmptyList.one(Result.usageError(s"$unformattedPattern is not a valid $name", pos)))
        }
    } yield res
  }

  /**
    * Parse a set for a paramter given the current argument list, and a list of the valid choices.
    */
  def parseMany[F[_]: Monad: ParserState: ParserError, A](
      name: String,
      choices: Iterable[A]
  )(implicit named: HasName[A]): F[Set[A]] =
    parseMany(name, choices.map(obj => named(obj).toLowercaseRoot -> obj).toMap)
}
