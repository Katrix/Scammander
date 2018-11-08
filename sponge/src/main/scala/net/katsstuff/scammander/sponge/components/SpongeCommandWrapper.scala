package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds

import java.util
import java.util.Optional
import javax.annotation.Nullable

import scala.collection.JavaConverters._

import cats.Eval
import cats.arrow.FunctionK
import cats.data.{NonEmptyList, StateT}
import cats.instances.either._
import cats.mtl.instances.all._
import cats.syntax.all._
import net.katsstuff.scammander.ScammanderTypes.{CommandFailureNEL, ParserError}
import net.katsstuff.scammander._
import org.spongepowered.api.Sponge
import org.spongepowered.api.command._
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

case class SpongeCommandWrapper[G[_]](
    command: ComplexCommand[G, CommandSource, Unit, Option[Location[World]], Int, SpongeCommandWrapper[G]],
    info: CommandInfo[G],
    runG: FunctionK[G, Either[CommandFailureNEL, ?]]
) extends CommandCallable
    with ComplexStaticChildCommand[G, CommandSource, Unit, Option[Location[World]], Int, SpongeCommandWrapper[G]] {

  type Result[A] = Either[CommandFailureNEL, A]
  type Parser[A] = StateT[Result, List[RawCmdArg], A]

  //At least in the tests, this recurse for ever if we don't construct it manually
  implicit private val E: ParserError[Parser] = raiseInd(
    stateMonadLayerControl,
    handleEither
  )

  override def process(source: CommandSource, arguments: String): CommandResult = {
    val args = ScammanderHelper.stringToRawArgsQuoted(arguments)

    if (args.nonEmpty && command.childrenMap.contains(args.head.content)) {
      val childCommand = command.childrenMap(args.head.content)
      if (childCommand.testPermission(source)) {
        childCommand.process(source, args.tail.mkString(" "))
      } else {
        throw new CommandPermissionException
      }
    } else {
      val res = command.runRaw[Parser](source, ()).runA(args).flatMap(runG(_))

      res match {
        case Right(CommandSuccess(count)) => CommandResult.successCount(count)
        case Left(NonEmptyList(CommandError(msg, true), Nil)) =>
          throw new CommandException(Text.of(msg).concat(Text.NEW_LINE).concat(getUsage(source)))
        case Left(NonEmptyList(CommandError(msg, false), Nil)) => throw new CommandException(Text.of(msg))
        case Left(NonEmptyList(CommandSyntaxError(msg, pos), Nil)) =>
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case Left(NonEmptyList(CommandUsageError(msg, pos), Nil)) =>
          //TODO: Custom exception
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case Left(nel) =>
          val usage =
            if (nel.exists(_.shouldShowUsage)) s"\nUsage: ${command.usage[Result](source).getOrElse("<error>")}" else ""
          val msg = s"${nel.map(_.msg).toList.mkString("\n")}$usage" //TODO: Better error here
          throw new CommandException(Text.of(msg))
      }
    }
  }

  override def getSuggestions(
      source: CommandSource,
      arguments: String,
      @Nullable targetPosition: Location[World]
  ): util.List[String] = {
    def headCount(arg: String) = command.children.flatMap(_.aliases).count(_.startsWith(arg))

    val args              = ScammanderHelper.stringToRawArgsQuoted(arguments)
    lazy val content      = args.head.content
    lazy val childCommand = command.childrenMap(content)
    val doChildCommand = if (args.nonEmpty && command.childrenMap.contains(content)) {
      if (headCount(content) > 1) args.lengthCompare(1) > 0 else true
    } else false

    if (doChildCommand && childCommand.testPermission(source)) {
      childCommand.getSuggestions(source, args.tail.map(_.content).mkString(" "), targetPosition)
    } else {
      val parse = ScammanderHelper.firstArgAndDrop[Parser].flatMapF[Boolean] { arg =>
        val isParsed =
          if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
          else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
        if (isParsed) true.pure else NonEmptyList.one(CommandError("Not child")).raiseError
      }
      val childSuggestions =
        ScammanderHelper.suggestions[Parser, Boolean](parse, Eval.now(command.childrenMap.keys)).runA(args)
      val paramSuggestions = command.suggestions[Parser](source, Option(targetPosition)).runA(args)
      val ret = childSuggestions match {
        case Right(suggestions) => paramSuggestions.map(suggestions ++ _)
        case Left(_)            => paramSuggestions
      }

      ret.getOrElse(Nil).asJava
    }
  }

  override def testPermission(source: CommandSource): Boolean = info.permission.forall(source.hasPermission)

  override def getShortDescription(source: CommandSource): Optional[Text] =
    runG(info.shortDescription(source)) match {
      case Left(nel)                => Optional.of(Text.of(nel.map(_.msg).toList.mkString("\n")))
      case Right(Some(description)) => Optional.of(description)
      case Right(None)              => Optional.empty()
    }

  override def getHelp(source: CommandSource): Optional[Text] = {
    runG(info.help(source)) match {
      case Left(nel)         => Optional.of(Text.of(nel.map(_.msg).toList.mkString("\n")))
      case Right(Some(help)) => Optional.of(help)
      case Right(None)       => Optional.empty()
    }
  }

  override def getUsage(source: CommandSource): Text =
    Text.of(command.usage[Result](source).getOrElse("<error>"))

  def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
    val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
    if (res.isPresent) Some(res.get()) else None
  }
}
