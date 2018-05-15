package net.katsstuff.scammander

import scala.language.higherKinds

case class ComplexChildCommand[F[_], Sender, Param, RootSender, RunExtra, TabExtra, Result, StaticChildCommand[
    ChildSender,
    ChildParam
] <: ComplexBaseStaticChildCommand[
  F,
  ChildSender,
  ChildParam,
  RootSender,
  RunExtra,
  TabExtra,
  Result,
  StaticChildCommand
]](aliases: Set[String], command: StaticChildCommand[Sender, Param])

trait ComplexBaseStaticChildCommand[F[_], Sender, Param, RootSender, RunExtra, TabExtra, Result, StaticChildCommand[
    ChildSender,
    ChildParam
] <: ComplexBaseStaticChildCommand[F, ChildSender, ChildParam, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]] {

  def command: ComplexCommand[F, Sender, Param, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]
}
