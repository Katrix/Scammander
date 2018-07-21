package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import org.bukkit.command.CommandSender

import net.katsstuff.scammander.{ComplexStaticChildCommand, ComplexCommand}

case class ChildCommandExtra[F[_]](
    commandWrapper: BukkitCommandWrapper[F],
    permission: Option[String],
    help: CommandSender => F[Option[String]],
    description: CommandSender => F[Option[String]]
) extends ComplexStaticChildCommand[F, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[F]] {

  override def command: ComplexCommand[F, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[F]] =
    commandWrapper.command
}
