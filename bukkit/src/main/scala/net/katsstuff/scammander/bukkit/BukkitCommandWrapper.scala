package net.katsstuff.scammander.bukkit

import java.util

import scala.collection.JavaConverters._

import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.plugin.java.JavaPlugin

import net.katsstuff.scammander.ScammanderHelper

case class BukkitCommandWrapper[Sender, Param](command: Command[Sender, Param]) extends TabExecutor {

  override def onCommand(
      source: CommandSender,
      bukkitCommand: BukkitCommand,
      label: String,
      args: Array[String]
  ): Boolean = {
    val extra = BukkitExtra(bukkitCommand, label)

    val res = for {
      sender <- command.userValidator.validate(source)
      param  <- command.par.parse(source, extra, ScammanderHelper.stringToRawArgs(args.mkString(" ")))
    } yield command.run(sender, extra, param._2)

    res.merge match {
      case CmdSuccess(_) => true
      case CmdError(msg) =>
        source.sendMessage(ChatColor.RED + msg)
        true
      case CmdSyntaxError(msg, _) =>
        //TODO: Show error location
        source.sendMessage(ChatColor.RED + msg)
        true
      case CmdUsageError(msg, _) =>
        //TODO: Show error location
        source.sendMessage(ChatColor.RED + msg)
        true
      case e: MultipleCmdErrors =>
        source.sendMessage(ChatColor.RED + e.msg) //TODO: Better error here
        true
    }
  }

  override def onTabComplete(
      sender: CommandSender,
      bukkitCommand: BukkitCommand,
      alias: String,
      args: Array[String]
  ): util.List[String] = {
    command.userValidator
      .validate(sender)
      .map(command.suggestions(_, ScammanderHelper.stringToRawArgs(args.mkString(" "))))
      .getOrElse(Nil)
      .asJava
  }

  def register(plugin: JavaPlugin, name: String): Unit = {
    val cmd = plugin.getCommand(name)
    cmd.setExecutor(this)
    cmd.setTabCompleter(this)
  }
}
