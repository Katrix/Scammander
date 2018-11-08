package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Monad
import cats.effect.Async
import net.katsstuff.scammander.ScammanderTypes.{ParserError, ParserState}

/**
  * The base class for a command. Extend from this if you want full control over a command.
  */
trait ComplexCommand[G[_], RootSender, RunExtra, TabExtra, ResultTpe, StaticChildCommand] {

  def runRaw[F[_]: Monad: ParserState: ParserError](
      source: RootSender,
      extra: RunExtra
  ): F[G[CommandSuccess[ResultTpe]]]

  def suggestions[F[_]: Async: ParserState: ParserError](
      source: RootSender,
      extra: TabExtra
  ): F[Seq[String]]

  def usage[F[_]: Monad: ParserError](source: RootSender): F[String]

  def children: Set[ComplexChildCommand[StaticChildCommand]] =
    Set.empty

  lazy val childrenMap: Map[String, StaticChildCommand] =
    children.flatMap(child => child.aliases.map(_ -> child.command)).toMap
}
