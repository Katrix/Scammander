package net.katsstuff.scammander.bukkit

import java.util

import scala.collection.JavaConverters._

import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}

trait BukkitUniverse extends ScammanderUniverse[CommandSender, BukkitExtra, BukkitExtra] {
  override def hasSenderPermission(sender: CommandSender, permission: String): Boolean =
    sender.hasPermission(permission)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toBukkit: BukkitCommandWrapper[Sender, Param] = BukkitCommandWrapper(command)

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player => Right(player)
    case _              => Left(CmdUsageError("This command can only be used by players", -1))
  }(identity)

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
        .map(
          source =>
            command.suggestions(
              source,
              BukkitExtra(bukkitCommand, alias),
              ScammanderHelper.stringToRawArgs(args.mkString(" "))
            )
        )
        .getOrElse(Nil)
        .asJava
    }

    def register(plugin: JavaPlugin, name: String): Unit = {
      val cmd = plugin.getCommand(name)
      cmd.setExecutor(this)
      cmd.setTabCompleter(this)
    }
  }
}