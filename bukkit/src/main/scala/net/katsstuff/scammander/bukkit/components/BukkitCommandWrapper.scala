package net.katsstuff.scammander.bukkit.components

import java.util

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.util.control.NonFatal

import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.plugin.java.JavaPlugin

import cats.{Eval, MonadError}
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.syntax.all._
import net.katsstuff.scammander._

case class BukkitCommandWrapper[F[_]](
    command: ComplexCommand[F, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[F]],
    runComputation: FunctionK[F, Either[NonEmptyList[CommandFailure], ?]]
)(implicit F: MonadError[F, NonEmptyList[CommandFailure]]) extends TabExecutor {

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
        val res   = command.runRaw(source, extra, ScammanderHelper.stringToRawArgsQuoted(args.mkString(" ")))

        runComputation(res) match {
          case Right(CommandSuccess(result)) => result
          case Left(NonEmptyList(CommandError(msg, true), Nil)) =>
            source.sendMessage(s"${ChatColor.RED}$msg\n${command.usage(source)}")
            true
          case Left(NonEmptyList(CommandError(msg, false), Nil)) =>
            source.sendMessage(ChatColor.RED + msg)
            true
          case Left(NonEmptyList(CommandSyntaxError(msg, _), Nil)) =>
            //TODO: Show error location
            val toSend = s"${ChatColor.RED}$msg\nUsage: ${command.usage(source)}"

            source.sendMessage(toSend)
            true
          case Left(NonEmptyList(CommandUsageError(msg, _), Nil)) =>
            //TODO: Show error location
            val toSend = s"${ChatColor.RED}$msg\nUsage: ${command.usage(source)}"

            source.sendMessage(toSend)
            true
          case Left(nel) =>
            val usage = if (nel.exists(_.shouldShowUsage)) s"\nUsage: ${command.usage(source)}" else ""
            source.sendMessage(s"${ChatColor.RED}${nel.map(_.msg).toList.mkString("\n")}$usage") //TODO: Better error here
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
      def headCount(arg: String) = command.children.flatMap(_.aliases).count(_.startsWith(arg))

      lazy val head         = args.head
      lazy val childCommand = command.childrenMap(head)

      val doChildCommand = if (args.nonEmpty && command.childrenMap.contains(head)) {
        if (headCount(head) > 1) args.lengthCompare(1) > 0 else true
      } else false

      if (doChildCommand && childCommand.permission.forall(sender.hasPermission)) {
        childCommand.commandWrapper.onTabComplete(sender, bukkitCommand, alias, args.tail)
      } else {
        val parsedArgs = ScammanderHelper.stringToRawArgsQuoted(args.mkString(" "))
        val extra      = BukkitExtra(bukkitCommand, alias)

        val parse = ScammanderHelper.firstArgAndDrop.flatMapF[Boolean] { arg =>
          val isParsed =
            if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
            else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
          if (isParsed) true.pure else F.raiseError(NonEmptyList.one(CommandError("Not child")))
        }
        val childSuggestions =
          ScammanderHelper.suggestions(parse, Eval.now(command.childrenMap.keys)).runA(parsedArgs)
        val paramSuggestions = command.suggestions(sender, extra, parsedArgs)
        val ret = runComputation(childSuggestions) match {
          case Right(suggestions) => paramSuggestions.map(suggestions ++ _)
          case Left(_)            => paramSuggestions
        }

        runComputation(ret).getOrElse(Nil).asJava
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
