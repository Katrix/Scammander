package net.katsstuff.scammander

import scala.language.higherKinds

case class ComplexChildCommand[F[_], RootSender, RunExtra, TabExtra, Result, StaticChildCommand <: ComplexBaseStaticChildCommand[
  F,
  RootSender,
  RunExtra,
  TabExtra,
  Result,
  StaticChildCommand
]](aliases: Set[String], command: StaticChildCommand)

trait ComplexBaseStaticChildCommand[F[_], RootSender, RunExtra, TabExtra, Result, StaticChildCommand <: ComplexBaseStaticChildCommand[
  F,
  RootSender,
  RunExtra,
  TabExtra,
  Result,
  StaticChildCommand
]] {

  def command: ComplexCommand[F, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]
}
