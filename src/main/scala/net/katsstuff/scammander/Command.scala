package net.katsstuff.scammander

abstract class Command[RootSender, CmdCtx, Sender, Param](
    implicit val senderTransformer: SenderTransformer[RootSender, Sender],
    val par: Parameter[RootSender, CmdCtx, Param]
) {

  def run(source: Sender, cmdCtx: CmdCtx, arg: Param): CmdResult

  def suggestions(source: Sender, strArgs: List[String]): Seq[String] =
    par.suggestions(senderTransformer.toSender(source), strArgs)._2

  def usage(source: Sender): String = par.usage
}
object Command {
  def simple[RootSender, CmdCtx, Sender, Param](runCmd: (Sender, CmdCtx, Param) => CmdResult)(
      implicit transformer: SenderTransformer[RootSender, Sender],
      parameter: Parameter[RootSender, CmdCtx, Param]
  ): Command[RootSender, CmdCtx, Sender, Param] =
    new Command[RootSender, CmdCtx, Sender, Param] {
      override def run(source: Sender, cmdCtx: CmdCtx, arg: Param): CmdResult = runCmd(source, cmdCtx, arg)
    }
}
