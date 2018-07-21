package net.katsstuff.scammander

import scala.language.higherKinds

case class ComplexChildCommand[StaticChildCommand](
    aliases: Set[String],
    command: StaticChildCommand
)

trait ComplexStaticChildCommand[F[_], RootSender, RunExtra, TabExtra, Result, StaticChildCommand] {

  def command: ComplexCommand[F, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]
}
