package net.katsstuff.scammander

import cats.MonadError
import cats.data.StateT
import cats.syntax.all._

trait HelpCommands {
  self: ScammanderBase with NormalParameters with HelperParameters =>

  type Title

  def sendMultipleCommandHelp(title: Title, source: RootSender, commands: Set[ChildCommand]): G[CommandSuccess]

  def sendCommandHelp(
      title: Title,
      source: RootSender,
      command: StaticChildCommand,
      path: List[String]
  ): G[CommandSuccess]

  def helpCommand(title: Title, commands: => Set[ChildCommand])(
      implicit G: MonadError[G, String]
  ): Command[RootSender, ZeroOrMore[String]] =
    Command.simple[ZeroOrMore[String]] { (source, extra, arg) =>
      val topCommandMap: Map[String, StaticChildCommand] =
        commands.flatMap(child => child.aliases.map(alias => alias -> child.command)).toMap

      def getChild(
          commandMap: Map[String, StaticChildCommand],
          name: String
      ): StateT[G, List[String], StaticChildCommand] = StateT { acc =>
        commandMap
          .get(name)
          .fold(G.raiseError[StaticChildCommand](s"No command named $name"))(_.pure[G])
          .map(cmd => (name :: acc) -> cmd)
      }

      arg match {
        case ZeroOrMore(Nil) =>
          sendMultipleCommandHelp(title, source, commands)
        case ZeroOrMore(Seq(head, tail @ _*)) =>
          val childCommandStep = getChild(topCommandMap, head).flatMap { fst =>
            import cats.instances.list._
            tail.toList.foldLeftM[StateT[G, List[String], ?], StaticChildCommand](fst) { (wrapper, childName) =>
              getChild(wrapper.command.childrenMap, childName)
            }
          }

          childCommandStep.run(Nil).flatMap {
            case (path, childCommand) => sendCommandHelp(title, source, childCommand, path.reverse)
          }
      }
    }
}
