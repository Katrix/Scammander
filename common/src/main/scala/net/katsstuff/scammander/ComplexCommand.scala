package net.katsstuff.scammander

import scala.language.higherKinds

/**
  * The base class for a command. Extend from this if you want full control over a command.
  */
abstract class ComplexCommand[F[_], Sender, Param, RootSender, RunExtra, TabExtra, Result, StaticChildCommand[
    ChildSender,
    ChildParam
] <: ComplexBaseStaticChildCommand[F, ChildSender, ChildParam, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]](
    implicit
    val userValidator: ComplexUserValidator[F, Sender, RootSender],
    val par: ComplexParameter[F, Param, RootSender, RunExtra, TabExtra]
) {

  def run(source: Sender, extra: RunExtra, arg: Param): F[CommandSuccess[Result]]

  def suggestions(source: RootSender, extra: TabExtra, strArgs: List[RawCmdArg]): F[Seq[String]]

  def usage(source: RootSender): F[String] = par.usage(source)

  def children: Set[ComplexChildCommand[F, _, _, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]] =
    Set.empty

  lazy val childrenMap: Map[String, StaticChildCommand[_, _]] =
    children.flatMap(child => child.aliases.map(alias => alias -> child.command)).toMap
}
