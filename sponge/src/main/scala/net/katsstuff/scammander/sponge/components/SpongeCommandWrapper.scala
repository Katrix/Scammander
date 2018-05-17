package net.katsstuff.scammander.sponge.components

import java.util
import java.util.Optional

import javax.annotation.Nullable

import scala.collection.JavaConverters._
import scala.language.higherKinds

import org.spongepowered.api.Sponge
import org.spongepowered.api.command._
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

import cats.MonadError
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.syntax.all._
import net.katsstuff.scammander._

case class SpongeCommandWrapper[F[_]](
    command: ComplexCommand[F, CommandSource, Unit, Option[Location[World]], Int, SpongeCommandWrapper[F]],
    info: CommandInfo,
    runComputation: FunctionK[F, ({ type L[A] = Either[NonEmptyList[CommandFailure], A] })#L]
)(implicit F: MonadError[F, NonEmptyList[CommandFailure]])
    extends CommandCallable
    with ComplexBaseStaticChildCommand[F, CommandSource, Unit, Option[Location[World]], Int, SpongeCommandWrapper[F]] {

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
      val res = command.runRootArgs(source, (), args)

      runComputation(res) match {
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
          val usage = if (nel.exists(_.shouldShowUsage)) s"\nUsage: ${command.usage(source)}" else ""
          val msg   = s"${nel.map(_.msg).toList.mkString("\n")}$usage" //TODO: Better error here
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
      val parse = ScammanderHelper.firstArgAndDrop.flatMapF[Boolean] { arg =>
        val isParsed =
          if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
          else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
        if (isParsed) true.pure else F.raiseError(NonEmptyList.one(CommandError("Not child")))
      }
      val childSuggestions =
        ScammanderHelper.suggestions(parse, command.childrenMap.keys).runA(args)
      val paramSuggestions = command.suggestions(source, Option(targetPosition), args)
      val ret = runComputation(childSuggestions) match {
        case Right(suggestions) => paramSuggestions.map(suggestions ++ _)
        case Left(_)            => paramSuggestions
      }

      runComputation(ret).getOrElse(Nil).asJava
    }
  }

  override def testPermission(source: CommandSource): Boolean = info.permission.forall(source.hasPermission)

  override def getShortDescription(source: CommandSource): Optional[Text] = info.shortDescription(source) match {
    case Some(description) => Optional.of(description)
    case None              => Optional.empty()
  }

  override def getHelp(source: CommandSource): Optional[Text] = info.help(source) match {
    case Some(help) => Optional.of(help)
    case None       => Optional.empty()
  }

  override def getUsage(source: CommandSource): Text = Text.of(runComputation(command.usage(source)))

  def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
    val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
    if (res.isPresent) Some(res.get()) else None
  }
}
