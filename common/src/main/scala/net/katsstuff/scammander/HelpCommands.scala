package net.katsstuff.scammander

import net.katsstuff.scammander.CrossCompatibility._

trait HelpCommands[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra]
    with NormalParameters[RootSender, RunExtra, TabExtra]
    with HelperParameters[RootSender, RunExtra, TabExtra] =>

  type Title

  def sendMultipleCommandHelp(title: Title, source: RootSender, commands: Set[ChildCommand[_, _]]): CommandStep[CommandSuccess]

  def sendCommandHelp(
      title: Title,
      source: RootSender,
      command: StaticChildCommand[_, _],
      path: List[String]
  ): CommandStep[CommandSuccess]

  def helpCommand(title: Title, commands: => Set[ChildCommand[_, _]]): Command[RootSender, ZeroOrMore[String]] =
    new Command[RootSender, ZeroOrMore[String]]() {
      private lazy val commandMap: Map[String, StaticChildCommand[_, _]] =
        commands.flatMap(child => child.aliases.map(alias => alias -> child.command)).toMap

      override def run(source: RootSender, extra: RunExtra, arg: ZeroOrMore[String]): CommandStep[CommandSuccess] =
        arg match {
          case ZeroOrMore(Nil) =>
            sendMultipleCommandHelp(title, source, commands)
          case ZeroOrMore(Seq(head, tail @ _*)) =>
            val first = commandMap.get(head).toStep(s"No command named $head").map(List(head) -> _)
            val childCommandStep = tail.foldLeft(first) {
              case (Right((acc, command)), child) =>
                command.command.childrenMap.get(child).toStep(s"No command named $head").map((child :: acc) -> _)
              case (e @ Left(_), _) => e
            }

            childCommandStep.flatMap {
              case (path, childCommand) => sendCommandHelp(title, source, childCommand, path.reverse)
            }
        }
    }
}
