package net.katsstuff.scammander

import scala.language.higherKinds

import cats.data.StateT
import cats.syntax.all._

trait HelpCommands[F[_]] {
  self: ScammanderBase[F]
    with NormalParameters[F]
    with HelperParameters[F] =>

  type Title

  def sendMultipleCommandHelp(title: Title, source: RootSender, commands: Set[ChildCommand]): F[CommandSuccess]

  def sendCommandHelp(
      title: Title,
      source: RootSender,
      command: StaticChildCommand,
      path: List[String]
  ): F[CommandSuccess]

  def helpCommand(title: Title, commands: => Set[ChildCommand]): Command[RootSender, ZeroOrMore[String]] =
    new Command[RootSender, ZeroOrMore[String]] {
      private lazy val topCommandMap: Map[String, StaticChildCommand] =
        commands.flatMap(child => child.aliases.map(alias => alias -> child.command)).toMap

      def getChild(
          commandMap: Map[String, StaticChildCommand],
          name: String
      ): StateT[F, List[String], StaticChildCommand] = StateT { acc =>
        commandMap
          .get(name)
          .toF(s"No command named $name")
          .map(cmd => (name :: acc) -> cmd)
      }

      override def run(source: RootSender, extra: RunExtra, arg: ZeroOrMore[String]): F[CommandSuccess] =
        arg match {
          case ZeroOrMore(Nil) =>
            sendMultipleCommandHelp(title, source, commands)
          case ZeroOrMore(Seq(head, tail @ _*)) =>
            val childCommandStep = getChild(topCommandMap, head).flatMap { fst =>
              import cats.instances.list._
              tail.toList.foldLeftM[StateT[F, List[String], ?], StaticChildCommand](fst) {
                (wrapper, childName) => getChild(wrapper.command.childrenMap, childName)
              }
            }

            childCommandStep.run(Nil).flatMap {
              case (path, childCommand) => sendCommandHelp(title, source, childCommand, path.reverse)
            }
        }
    }
}
