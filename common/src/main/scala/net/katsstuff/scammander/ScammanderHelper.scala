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

import net.katsstuff.scammander.misc.RawCmdArg

object ScammanderHelper {

  private val spaceRegex = """\S+""".r //TODO: Support quoted arguments

  val notEnoughArgs = CommandSyntaxError("Not enough arguments", -1)

  def stringToRawArgs(arguments: String): List[RawCmdArg] =
    spaceRegex.findAllMatchIn(arguments).map(m => RawCmdArg(m.start, m.end, m.matched)).toList

  def suggestions(xs: List[RawCmdArg], choices: => Iterable[String]): (List[RawCmdArg], Seq[String]) = {
    val head = xs.head
    val tail = xs.tail

    if (tail.isEmpty) (Nil, choices.filter(head.content.startsWith).toSeq) else (tail, Nil)
  }

  def parse[A](
      name: String,
      xs: List[RawCmdArg],
      choices: Map[String, A]
  ): Either[CommandFailure, (List[RawCmdArg], A)] = {
    if (xs.nonEmpty) {
      val head = xs.head
      choices
        .get(head.content.toLowerCase(Locale.ROOT))
        .toRight(CommandUsageError(s"${head.content} is not a valid $name", head.start))
        .map(xs.tail -> _)
    } else Left(notEnoughArgs)
  }

  //Based on PatternMatchingCommandElement in Sponge
  def parseMany[A](
      name: String,
      xs: List[RawCmdArg],
      choices: Map[String, A]
  ): Either[CommandFailure, (List[RawCmdArg], Set[A])] = {
    def formattedPattern(input: String) = {
      // Anchor matches to the beginning -- this lets us use find()
      val usedInput = if (!input.startsWith("^")) s"^$input" else input
      Pattern.compile(usedInput, Pattern.CASE_INSENSITIVE)
    }

    if (xs.nonEmpty) {
      val RawCmdArg(pos, _, unformattedPattern) = xs.head

      val pattern         = formattedPattern(unformattedPattern)
      val filteredChoices = choices.filterKeys(k => pattern.matcher(k).find())
      filteredChoices
        .collectFirst {
          case (k, v) if k.equalsIgnoreCase(unformattedPattern) => Right(xs.tail -> Set(v))
        }
        .getOrElse {
          if (filteredChoices.nonEmpty) {
            Right(xs.tail -> filteredChoices.values.toSet)
          } else Left(CommandUsageError(s"No values present for $unformattedPattern", pos))
        }
    } else Left(notEnoughArgs)
  }
}
