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

sealed trait CmdResult
object CmdResult {
  def success(count: Int = 1): CmdSuccess = CmdSuccess(count)
  def error(msg: String):      CmdError   = CmdError(msg)
}
case class CmdSuccess(count: Int) extends CmdResult
sealed trait CmdFailure extends CmdResult {
  def msg: String
  def merge(failure: CmdFailure): CmdFailure = MultipleCmdErrors(Seq(this, failure))
}
object CmdFailure {
  def error(msg: String)                 = CmdError(msg)
  def syntaxError(msg: String, pos: Int) = CmdSyntaxError(msg, pos)
  def usageError(msg: String, pos: Int)  = CmdUsageError(msg, pos)
}
case class CmdError(msg: String)                      extends CmdFailure
case class CmdSyntaxError(msg: String, position: Int) extends CmdFailure
case class CmdUsageError(msg: String, position: Int)  extends CmdFailure
case class MultipleCmdErrors(failures: Seq[CmdFailure]) extends CmdFailure {
  override def merge(failure: CmdFailure): CmdFailure = MultipleCmdErrors(failures :+ failure)

  //We don't want to show too many errors
  override def msg: String = failures.take(5).map(_.msg).mkString("\n")
}
