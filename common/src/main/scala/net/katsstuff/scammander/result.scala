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

sealed trait CommandResult
object CommandResult {
  def success(count: Int = 1): CommandSuccess = CommandSuccess(count)
  def error(msg: String):      CommandError   = CommandError(msg)
}
case class CommandSuccess(count: Int = 1) extends CommandResult
sealed trait CommandFailure extends CommandResult {
  def msg: String
  def merge(failure: CommandFailure): CommandFailure = MultipleCommandErrors(Seq(this, failure))
}
object CommandFailure {
  def error(msg: String)                 = CommandError(msg)
  def syntaxError(msg: String, pos: Int) = CommandSyntaxError(msg, pos)
  def usageError(msg: String, pos: Int)  = CommandUsageError(msg, pos)
}
case class CommandError(msg: String)                      extends CommandFailure
case class CommandSyntaxError(msg: String, position: Int) extends CommandFailure
case class CommandUsageError(msg: String, position: Int)  extends CommandFailure
case class MultipleCommandErrors(failures: Seq[CommandFailure]) extends CommandFailure {
  override def merge(failure: CommandFailure): CommandFailure = MultipleCommandErrors(failures :+ failure)

  //We don't want to show too many errors
  override def msg: String = failures.take(5).map(_.msg).mkString("\n")
}
