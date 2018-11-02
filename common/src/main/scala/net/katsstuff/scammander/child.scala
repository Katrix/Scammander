package net.katsstuff.scammander

import scala.language.higherKinds

case class ComplexChildCommand[StaticChildCommand](
    aliases: Set[String],
    command: StaticChildCommand
)

trait ComplexStaticChildCommand[G[_], RootSender, RunExtra, TabExtra, ResultTpe, StaticChildCommand] {

  def command: ComplexCommand[G, RootSender, RunExtra, TabExtra, ResultTpe, StaticChildCommand]
}
