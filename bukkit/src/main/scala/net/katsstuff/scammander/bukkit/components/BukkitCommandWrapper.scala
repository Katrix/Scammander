package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import java.util

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

import cats.arrow.FunctionK
import cats.data.{EitherT, NonEmptyList, StateT}
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.instances.either._
import cats.syntax.all._
import cats.instances.vector._
import cats.mtl.instances.all._
import cats.mtl.{DefaultMonadState, MonadState}
import cats.{Eval, Monad}
import net.katsstuff.scammander.ScammanderTypes.{CommandFailureNEL, ParserError}
import net.katsstuff.scammander._
import org.bukkit.ChatColor
import org.bukkit.command.{CommandSender, TabExecutor, Command => BukkitCommand}
import org.bukkit.plugin.java.JavaPlugin

case class BukkitCommandWrapper[G[_]](
    command: ComplexCommand[G, CommandSender, BukkitExtra, BukkitExtra, Boolean, ChildCommandExtra[G]],
    runG: FunctionK[G, Either[CommandFailureNEL, ?]]
) extends TabExecutor {

  private type Result[A] = Either[CommandFailureNEL, A]
  private type Parser[A] = StateT[Result, List[RawCmdArg], A]

  private type ResultIO[A] = EitherT[IO, CommandFailureNEL, A]

  //At least in the tests, this recurse for ever if we don't construct it manually
  implicit private val E: ParserError[Parser] = raiseInd(
    stateMonadLayerControl,
    handleEither
  )

  private def ioState[S](start: S): IO[MonadState[IO, S]] =
    Ref[IO]
      .of(start)
      .map { ref =>
        new DefaultMonadState[IO, S] {
          override val monad: Monad[IO]    = IO.ioEffect
          override def get: IO[S]          = ref.get
          override def set(s: S): IO[Unit] = ref.set(s)
        }
      }

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
        val extra      = BukkitExtra(bukkitCommand, label)
        val parsedArgs = ScammanderHelper.stringToRawArgsQuoted(args.mkString(" "))
        val res        = command.runRaw[Parser](source, extra).runA(parsedArgs).flatMap(runG(_))

        res match {
          case Right(CommandSuccess(result)) => result
          case Left(NonEmptyList(CommandError(msg, true), Nil)) =>
            source.sendMessage(s"${ChatColor.RED}$msg${command.usage[Result](source).fold(_ => "", "\n" + _)}")
            true
          case Left(NonEmptyList(CommandError(msg, false), Nil)) =>
            source.sendMessage(ChatColor.RED + msg)
            true
          case Left(NonEmptyList(CommandSyntaxError(msg, _), Nil)) =>
            //TODO: Show error location
            val toSend = s"${ChatColor.RED}$msg\nUsage: ${command.usage[Result](source).getOrElse("<error>")}"

            source.sendMessage(toSend)
            true
          case Left(NonEmptyList(CommandUsageError(msg, _), Nil)) =>
            //TODO: Show error location
            val toSend = s"${ChatColor.RED}$msg\nUsage: ${command.usage[Result](source).getOrElse("<error>")}"

            source.sendMessage(toSend)
            true
          case Left(nel) =>
            val usage =
              if (nel.exists(_.shouldShowUsage)) s"\nUsage: ${command.usage[Result](source).getOrElse("<error>")}"
              else ""
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

        val parse = ScammanderHelper.firstArgAndDrop[Parser].flatMapF[Boolean] { arg =>
          val isParsed =
            if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
            else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
          if (isParsed) Right(true) else Left(NonEmptyList.one(CommandError("Not child")))
        }
        val childSuggestions =
          ScammanderHelper
            .suggestions[Parser, Boolean](parse, Eval.now(command.childrenMap.keys))
            .runA(parsedArgs)
            .map(_.toVector)

        ioState(parsedArgs)
          .flatMap(implicit state => command.suggestions[ResultIO](sender, extra).map(_.toVector).value)
          .map(_ |+| childSuggestions)
          .map(_.getOrElse(Nil))
          .map(_.asJava)
          .unsafeRunSync() //TODO: Don't run sync once we update to 1.13
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
