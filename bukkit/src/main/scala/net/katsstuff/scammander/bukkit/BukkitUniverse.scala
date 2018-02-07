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

import java.net.InetAddress
import java.util

import scala.collection.JavaConverters._

import org.bukkit.command.{
  BlockCommandSender,
  CommandSender,
  ProxiedCommandSender,
  TabExecutor,
  Command => BukkitCommand
}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.plugin.Plugin
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Bukkit, ChatColor, Location, OfflinePlayer, World}

import net.katsstuff.scammander.misc.{HasName, RawCmdArg}
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import shapeless._

trait BukkitUniverse extends ScammanderUniverse[CommandSender, BukkitExtra, BukkitExtra] {

  override protected type Result             = Boolean
  override protected type StaticChildCommand = ChildCommand

  case class ChildCommand(command: BukkitCommandWrapper[_, _], permission: String, help: String, description: String)

  override protected val defaultCommandSuccess: Boolean = true

  override protected def tabExtraToRunExtra(extra: BukkitExtra): BukkitExtra = extra

  implicit val playerHasName:        HasName[Player]        = (a: Player) => a.getName
  implicit val offlinePlayerHasName: HasName[OfflinePlayer] = (a: OfflinePlayer) => a.getName
  implicit val worldHasName:         HasName[World]         = (a: World) => a.getName
  implicit val pluginHasName:        HasName[Plugin]        = (a: Plugin) => a.getName

  /**
    * A class to use for parameter that should require a specific permission.
    */
  case class NeedPermission[S <: String, A](value: A)

  implicit def needPermissionParam[S <: String, A](
      implicit param0: Parameter[A],
      w: Witness.Aux[S]
  ): Parameter[NeedPermission[S, A]] =
    new ProxyParameter[NeedPermission[S, A], A] {
      override def param: Parameter[A] = param0

      val perm: String = w.value

      override def parse(
          source: CommandSender,
          extra: BukkitExtra,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], NeedPermission[S, A])] =
        if (source.hasPermission(perm)) param.parse(source, extra, xs).map(t => t._1 -> NeedPermission(t._2))
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
      ): Either[List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else Left(xs.drop(1))
    }

  //TODO: Selector with NMS
  implicit val allPlayerParam: Parameter[Set[Player]] = Parameter.mkNamed("player", Bukkit.getOnlinePlayers.asScala)

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Player)] = param.parse(source, extra, xs).map(t => t._1 -> t._2.value)
  }

  //TODO: Entity selector with NMS

  implicit val offlinePlayerParam: Parameter[Set[OfflinePlayer]] = new Parameter[Set[OfflinePlayer]] {
    override def name: String = "offlinePlayer"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[OfflinePlayer])] = {
      val players = allPlayerParam.parse(source, extra, xs)
      val users   = ScammanderHelper.parseMany(name, xs, Bukkit.getOfflinePlayers)

      for {
        e1 <- players.map(t => t._1 -> t._2.map(player => player: OfflinePlayer)).left
        e2 <- users.left
      } yield e1.merge(e2)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] =
      ScammanderHelper.suggestionsNamed(parse(source, extra, _), xs, Bukkit.getOfflinePlayers)
  }

  implicit val worldParam: Parameter[Set[World]] = Parameter.mkNamed("world", Bukkit.getWorlds.asScala)

  implicit val vector3dParam: Parameter[BukkitVector] = new Parameter[BukkitVector] {
    override def name: String = "vector3d"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], BukkitVector)] = {
      val relative = source match {
        case entity: Entity                  => Some(entity.getLocation)
        case blockSender: BlockCommandSender => Some(blockSender.getBlock.getLocation)
        case _                               => None
      }

      for {
        tx <- parseRelativeDouble(source, extra, xs, relative.map(_.getX))
        ty <- parseRelativeDouble(source, extra, tx._1, relative.map(_.getY))
        tz <- parseRelativeDouble(source, extra, ty._1, relative.map(_.getZ))
      } yield tz._1 -> new BukkitVector(tx._2, ty._2, tz._2)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(3))

    private def parseRelativeDouble(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg],
        relativeToOpt: Option[Double]
    ): CommandStep[(List[RawCmdArg], Double)] = {
      val RawCmdArg(start, end, arg) = xs.head

      if (arg.startsWith("~")) {
        relativeToOpt
          .toRight(Command.usageError("Relative position specified but source does not have a position", start))
          .flatMap { relativeTo =>
            val newArg = arg.substring(1)
            if (newArg.isEmpty) Right(xs.tail -> relativeTo)
            else {
              doubleParam.parse(source, extra, RawCmdArg(start, end, newArg) :: xs.tail).map {
                case (ys, res) =>
                  ys -> (res + relativeTo)
              }
            }
          }
      } else {
        doubleParam.parse(source, extra, xs)
      }
    }
  }

  implicit val pluginParam: Parameter[Set[Plugin]] = Parameter.mkNamed("plugin", Bukkit.getPluginManager.getPlugins)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toBukkit: BukkitCommandWrapper[Sender, Param] = BukkitCommandWrapper(command)
    def toBukkitChild(permission: String = "", help: String = "", description: String = ""): ChildCommand =
      ChildCommand(command.toBukkit, permission, help, description)

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player => Right(player)
    case _              => Left(CommandUsageError("This command can only be used by players", -1))
  }

  implicit val offlinePlayerSender: UserValidator[OfflinePlayer] = UserValidator.mkValidator {
    case player: OfflinePlayer => Right(player)
    case _                     => Left(CommandUsageError("This command can only be used by players", -1))
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity)          => Right(entity)
      case proxy: ProxiedCommandSender => entitySender.validate(proxy.getCaller)
      case _                           => Left(CommandUsageError("This command can only be used by players", -1))
    }
  }

  implicit val locationSender: UserValidator[Location] = UserValidator.mkValidator {
    case entity: Entity                  => Right(entity.getLocation)
    case blockSender: BlockCommandSender => Right(blockSender.getBlock.getLocation)
    case proxy: ProxiedCommandSender     => locationSender.validate(proxy.getCaller)
    case _                               => Left(CommandUsageError("This command can only be used by things which have a location", -1))
  }

  implicit val vector3dSender: UserValidator[BukkitVector] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.toVector))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case player: Player              => Right(player.getAddress.getAddress)
    case proxy: ProxiedCommandSender => ipSender.validate(proxy.getCaller)
    case _                           => Left(CommandUsageError("This command can only be used by things which have an IP", -1))
  }

  case class BukkitCommandWrapper[Sender, Param](command: Command[Sender, Param]) extends TabExecutor {

    override def onCommand(
        source: CommandSender,
        bukkitCommand: BukkitCommand,
        label: String,
        args: Array[String]
    ): Boolean = {
      if (args.nonEmpty && command.children.contains(args.head)) {
        val childCommand = command.children(args.head)
        if (childCommand.permission.isEmpty || source.hasPermission(childCommand.permission)) {
          childCommand.command.onCommand(source, bukkitCommand, label, args.tail)
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
    }

    override def onTabComplete(
        sender: CommandSender,
        bukkitCommand: BukkitCommand,
        alias: String,
        args: Array[String]
    ): util.List[String] = {
      if (args.nonEmpty && command.children.contains(args.head)) {
        val childCommand = command.children(args.head)
        if (childCommand.permission.isEmpty || sender.hasPermission(childCommand.permission)) {
          childCommand.command.onTabComplete(sender, bukkitCommand, alias, args.tail)
        } else {
          Nil.asJava
        }
      } else {
        val parsedArgs = ScammanderHelper.stringToRawArgsQuoted(args.mkString(" "))
        val extra      = BukkitExtra(bukkitCommand, alias)

        val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = xs => {
          val isParsed = xs.headOption.exists(arg => command.children.keys.exists(_.equalsIgnoreCase(arg.content)))
          Either.cond(isParsed, (xs.drop(1), true), Command.error("Not child"))
        }
        val childSuggestions = ScammanderHelper.suggestions(parse, parsedArgs, command.children.keys)
        val ret =
          if (parsedArgs.nonEmpty && childSuggestions.isRight) childSuggestions.getOrElse(Nil)
          else {
            val paramSuggestions = command.suggestions(sender, extra, parsedArgs)
            childSuggestions.getOrElse(Nil) ++ paramSuggestions
          }

        ret.asJava
      }
    }

    def register(plugin: JavaPlugin, name: String): Unit = {
      val cmd = plugin.getCommand(name)
      cmd.setExecutor(this)
      cmd.setTabCompleter(this)
    }
  }
}
