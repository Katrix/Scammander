package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Foldable

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

      override def run(source: RootSender, extra: RunExtra, arg: ZeroOrMore[String]): F[CommandSuccess] =
        arg match {
          case ZeroOrMore(Nil) =>
            sendMultipleCommandHelp(title, source, commands)
          case ZeroOrMore(Seq(head, tail @ _*)) =>
            val first = F.map(commandMap.get(head).toF(s"No command named $head"))(List(head) -> _)

            val childCommandStep = F.flatMap(first) { fst =>
              import cats.instances.list._
              Foldable[List].foldLeftM[F, String, (List[String], StaticChildCommand[_, _])](tail.toList, fst) {
                case ((acc, command), child) =>
                  F.map(command.command.childrenMap.get(child).toF(s"No command named $child"))(
                    cmd => (child :: acc) -> cmd
                  )
              }
            }

            F.flatMap(childCommandStep) {
              case (path, childCommand) => sendCommandHelp(title, source, childCommand, path.reverse)
            }
        }
    }
}
