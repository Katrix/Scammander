package net.katsstuff.scammander

abstract class Command[RootSender, Sender, Param](
    implicit val senderTransformer: SenderTransformer[RootSender, Sender],
    val par: Parameter[RootSender, Param]
) {

  def run(source: Sender, arg: Param): CmdResult

  def suggestions(source: Sender, strArgs: List[String]): Seq[String] =
    par.suggestions(senderTransformer.toSender(source), strArgs)._2

  def usage(source: Sender): String = par.usage
}
object Command {
  def simple[RootSender, Sender, Param](runCmd: (Sender, Param) => CmdResult)(
      implicit transformer: SenderTransformer[RootSender, Sender],
      parameter: Parameter[RootSender, Param]
  ): Command[RootSender, Sender, Param] =
    new Command[RootSender, Sender, Param] {
      override def run(source: Sender, arg: Param): CmdResult = runCmd(source, arg)
    }
}
