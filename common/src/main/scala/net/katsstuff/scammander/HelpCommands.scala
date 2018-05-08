package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Foldable
import cats.syntax.all._

trait HelpCommands[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra]
    with NormalParameters[F, RootSender, RunExtra, TabExtra]
    with HelperParameters[F, RootSender, RunExtra, TabExtra] =>

  type Title

  def sendMultipleCommandHelp(title: Title, source: RootSender, commands: Set[ChildCommand[_, _]]): F[CommandSuccess]

  def sendCommandHelp(
      title: Title,
      source: RootSender,
      command: StaticChildCommand[_, _],
      path: List[String]
  ): F[CommandSuccess]

  def helpCommand(title: Title, commands: => Set[ChildCommand[_, _]]): Command[RootSender, ZeroOrMore[String]] =
    new Command[RootSender, ZeroOrMore[String]]() {
      private lazy val commandMap: Map[String, StaticChildCommand[_, _]] =
        commands.flatMap(child => child.aliases.map(alias => alias -> child.command)).toMap

      def getCommandOrError(
          commandMap: Map[String, StaticChildCommand[_, _]],
          acc: List[String],
          name: String
      ): F[(List[String], StaticChildCommand[_, _])] =
        commandMap
          .get(name)
          .toF(s"No command named $name")
          .map(cmd => (name :: acc) -> cmd)

      override def run(source: RootSender, extra: RunExtra, arg: ZeroOrMore[String]): F[CommandSuccess] =
        arg match {
          case ZeroOrMore(Nil) =>
            sendMultipleCommandHelp(title, source, commands)
          case ZeroOrMore(Seq(head, tail @ _*)) =>
            val first = getCommandOrError(commandMap, Nil, head)

            val childCommandStep = first.flatMap { fst =>
              import cats.instances.list._
              Foldable[List].foldLeftM[F, String, (List[String], StaticChildCommand[_, _])](tail.toList, fst) {
                case ((acc, wrapper), child) =>
                  getCommandOrError(wrapper.command.childrenMap, acc, child)
              }
            }

            childCommandStep.flatMap {
              case (path, childCommand) => sendCommandHelp(title, source, childCommand, path.reverse)
            }
        }
    }
}
