/*
 * This file is part of Scammander, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katsstuff.scammander.bukkit

import java.util

import scala.collection.JavaConverters._

import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

import net.katsstuff.scammander.misc.RawCmdArg
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import shapeless._

trait BukkitUniverse extends ScammanderUniverse[CommandSender, BukkitExtra, BukkitExtra] {

  case class NeedPermission[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[A, A] {
    val perm: String = w.value

    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], A)] =
      if (source.hasPermission(perm)) param.parse(source, extra, xs)
      else
        Left(
          CommandUsageError(
            "You do not have the permissions needed to use this parameter",
            xs.headOption.map(_.start).getOrElse(-1)
          )
        )

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) =
      if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else (xs.tail, Nil)
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toBukkit: BukkitCommandWrapper[Sender, Param] = BukkitCommandWrapper(command)

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player => Right(player)
    case _              => Left(CommandUsageError("This command can only be used by players", -1))
  }

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
        case CommandSuccess(_) => true
        case CommandError(msg) =>
          source.sendMessage(ChatColor.RED + msg)
          true
        case CommandSyntaxError(msg, _) =>
          //TODO: Show error location
          source.sendMessage(ChatColor.RED + msg)
          true
        case CommandUsageError(msg, _) =>
          //TODO: Show error location
          source.sendMessage(ChatColor.RED + msg)
          true
        case e: MultipleCommandErrors =>
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
      command.suggestions(
        sender,
        BukkitExtra(bukkitCommand, alias),
        ScammanderHelper.stringToRawArgs(args.mkString(" "))
      ).asJava
    }

    def register(plugin: JavaPlugin, name: String): Unit = {
      val cmd = plugin.getCommand(name)
      cmd.setExecutor(this)
      cmd.setTabCompleter(this)
    }
  }
}
