package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import net.katsstuff.scammander.{ComplexCommand, ComplexStaticChildCommand}
import org.bukkit.command.CommandSender

case class ChildCommandExtra[G[_]](
    commandWrapper: BukkitCommandWrapper[G],
    permission: Option[String],
    help: CommandSender => G[Option[String]],
    description: CommandSender => G[Option[String]]
) extends ComplexStaticChildCommand[G, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[G]] {

  override def command: ComplexCommand[G, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[G]] =
    commandWrapper.command
}
