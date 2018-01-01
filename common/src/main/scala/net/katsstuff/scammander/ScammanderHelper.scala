package net.katsstuff.scammander

import net.katsstuff.scammander.misc.RawCmdArg

object ScammanderHelper {

  private val spaceRegex = """\S+""".r //TODO: Support quoted arguments

  def stringToRawArgs(arguments: String): List[RawCmdArg] =
    spaceRegex.findAllMatchIn(arguments).map(m => RawCmdArg(m.start, m.end, m.matched)).toList
}
