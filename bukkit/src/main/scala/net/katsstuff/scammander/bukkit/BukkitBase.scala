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
import scala.util.control.NonFatal

import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.plugin.java.JavaPlugin

import net.katsstuff.scammander.{ScammanderBase, ScammanderHelper}

trait BukkitBase extends ScammanderBase[CommandSender, BukkitExtra, BukkitExtra] {

  override protected type Result                            = Boolean
  override protected type StaticChildCommand[Sender, Param] = ChildCommandExtra[Sender, Param]

  case class ChildCommandExtra[Sender, Param](
      commandWrapper: BukkitCommandWrapper[Sender, Param],
      permission: Option[String],
      help: CommandSender => Option[String],
      description: CommandSender => Option[String]
  ) extends SharedStaticChildCommand[Sender, Param] {

    override def command: Command[Sender, Param] = commandWrapper.command
  }

  override protected val defaultCommandSuccess: Boolean = true

  override protected def tabExtraToRunExtra(extra: BukkitExtra): BukkitExtra = extra

  /**
    * Helper for creating an alias when registering a command.
    */
  object Alias {
    def apply(first: String, aliases: String*): Set[String] = aliases.toSet + first
  }

  /**
    * Helper for creating a alias when registering a command.
    */
  object Permission {
    def apply(perm: String): Some[String] = Some(perm)
    val none:                None.type    = None
  }

  /**
    * Helper for creating a help when registering a command.
    */
  object Help {
    def apply(f: CommandSender => String): CommandSender => Option[String] = f andThen Some.apply
    def apply(help: String):               CommandSender => Option[String] = _ => Some(help)
    val none:                              CommandSender => None.type      = _ => None
  }

  /**
    * Helper for creating an description when registering a command.
    */
  object Description {
    def apply(f: CommandSender => String): CommandSender => Option[String] = f andThen Some.apply
    def apply(description: String):        CommandSender => Option[String] = _ => Some(description)
    val none:                              CommandSender => None.type      = _ => None
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toBukkit: BukkitCommandWrapper[Sender, Param] = BukkitCommandWrapper(command)
    def toChild(
        aliases: Set[String],
        permission: Option[String] = None,
        help: CommandSender => Option[String] = _ => None,
        description: CommandSender => Option[String] = _ => None
    ): ChildCommand[Sender, Param] =
      ChildCommand(aliases, ChildCommandExtra(command.toBukkit, permission, help, description))

    def toRootChild(plugin: JavaPlugin, name: String): ChildCommand[Sender, Param] = {
      val bukkitCommand = plugin.getCommand(name)

      ChildCommand(
        bukkitCommand.getAliases.asScala.toSet + bukkitCommand.getName,
        ChildCommandExtra(
          toBukkit,
          Option(bukkitCommand.getPermission),
          Help.none,
          if (bukkitCommand.getDescription.isEmpty) Description.none else Description(bukkitCommand.getDescription)
        )
      )
    }

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }

  case class BukkitCommandWrapper[Sender, Param](command: Command[Sender, Param]) extends TabExecutor {

    override def onCommand(
        source: CommandSender,
        bukkitCommand: BukkitCommand,
        label: String,
        args: Array[String]
    ): Boolean = {
      try {
        if (args.nonEmpty && command.childrenMap.contains(args.head)) {
          val childCommand = command.childrenMap(args.head)
          if (childCommand.permission.forall(source.hasPermission)) {
            childCommand.commandWrapper.onCommand(source, bukkitCommand, label, args.tail)
          } else {
            source.sendMessage(ChatColor.RED + "You don't have permission to use that command")
            false
          }
        } else {
          val extra = BukkitExtra(bukkitCommand, label)
          val res = for {
            sender <- command.userValidator.validate(source)
            param  <- command.par.parse(source, extra, ScammanderHelper.stringToRawArgsQuoted(args.mkString(" ")))
            result <- command.run(sender, extra, param._2)
          } yield result

          res match {
            case Right(CommandSuccess(result)) => result
            case Left(CommandError(msg)) =>
              source.sendMessage(ChatColor.RED + msg)
              true
            case Left(CommandSyntaxError(msg, _)) =>
              //TODO: Show error location
              source.sendMessage(ChatColor.RED + msg)
              true
            case Left(CommandUsageError(msg, _)) =>
              //TODO: Show error location
              source.sendMessage(ChatColor.RED + msg)
              true
            case Left(e: MultipleCommandErrors) =>
              source.sendMessage(ChatColor.RED + e.msg) //TODO: Better error here
              true
          }
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
          false
      }
    }

    override def onTabComplete(
        sender: CommandSender,
        bukkitCommand: BukkitCommand,
        alias: String,
        args: Array[String]
    ): util.List[String] = {
      try {
        lazy val head              = args.head
        lazy val childCommand      = command.childrenMap(head)
        def headCount(arg: String) = command.children.flatMap(_.aliases).count(_.startsWith(arg))
        val doChildCommand = if (args.nonEmpty && command.childrenMap.contains(head)) {
          if (headCount(head) > 1) args.lengthCompare(1) > 0 else true
        } else false

        if (doChildCommand && childCommand.permission.forall(sender.hasPermission)) {
          childCommand.commandWrapper.onTabComplete(sender, bukkitCommand, alias, args.tail)
        } else {
          val parsedArgs = ScammanderHelper.stringToRawArgsQuoted(args.mkString(" "))
          val extra      = BukkitExtra(bukkitCommand, alias)

          val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = xs => {
            val isParsed = xs.headOption.exists { arg =>
              if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
              else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
            }
            Either.cond(isParsed, (xs.drop(1), true), Command.error("Not child"))
          }
          val childSuggestions = ScammanderHelper.suggestions(parse, parsedArgs, command.childrenMap.keys)
          val paramSuggestions = command.suggestions(sender, extra, parsedArgs)
          val ret = childSuggestions match {
            case Right(suggestions) => suggestions ++ paramSuggestions
            case Left(_)            => paramSuggestions
          }

          ret.asJava
        }
      } catch {
        case NonFatal(e) =>
          e.printStackTrace()
          Nil.asJava
      }
    }

    def register(plugin: JavaPlugin, name: String): Unit = {
      val cmd = plugin.getCommand(name)
      cmd.setExecutor(this)
      cmd.setTabCompleter(this)
    }

    def unregister(plugin: JavaPlugin, name: String): Unit = {
      val cmd = plugin.getCommand(name)
      cmd.setExecutor(null)
      cmd.setTabCompleter(null)
    }
  }
}
