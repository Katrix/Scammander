package net.katsstuff.scammander

import scala.language.higherKinds

/**
  * The base class for a command. Extend from this if you want full control over a command.
  */
trait ComplexCommand[F[_], RootSender, RunExtra, TabExtra, Result, StaticChildCommand] {

  def runRaw(source: RootSender, extra: RunExtra, args: List[RawCmdArg]): F[CommandSuccess[Result]]

  def suggestions(source: RootSender, extra: TabExtra, args: List[RawCmdArg]): F[Seq[String]]

  def usage(source: RootSender): F[String]

  def children: Set[ComplexChildCommand[F, StaticChildCommand]] =
    Set.empty

  lazy val childrenMap: Map[String, StaticChildCommand] =
    children.flatMap(child => child.aliases.map(_ -> child.command)).toMap
}
