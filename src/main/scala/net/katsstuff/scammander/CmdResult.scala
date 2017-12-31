package net.katsstuff.scammander

sealed trait CmdResult
case class CmdError(msg: String) extends CmdResult
case class CmdSuccess(count: Int) extends CmdResult
