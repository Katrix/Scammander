package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import org.bukkit.command.CommandSender

import net.katsstuff.scammander.{ComplexBaseStaticChildCommand, ComplexCommand}

case class ChildCommandExtra[F[_]](
    commandWrapper: BukkitCommandWrapper[F],
    permission: Option[String],
    help: CommandSender => Option[String],
    description: CommandSender => Option[String]
) extends ComplexBaseStaticChildCommand[F, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[F]] {

  override def command: ComplexCommand[F, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[F]] =
    commandWrapper.command
}
