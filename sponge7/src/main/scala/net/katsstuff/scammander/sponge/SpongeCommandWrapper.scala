package net.katsstuff.scammander.sponge

import java.util
import java.util.Optional

import scala.collection.JavaConverters._

import org.spongepowered.api.Sponge
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.command.{CommandCallable, CommandException, CommandMapping, CommandResult, CommandSource}
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

import net.katsstuff.scammander.ScammanderHelper

case class SpongeCommandWrapper[Sender, Param](command: Command[Sender, Param], extra: SpongeCommandInfo)
    extends CommandCallable {

  override def process(source: CommandSource, arguments: String): CommandResult = {
    val res = for {
      sender <- command.userValidator.validate(source)
      param  <- command.par.parse(source, (), ScammanderHelper.stringToRawArgs(arguments))
    } yield command.run(sender, (), param._2)

    res.merge match {
      case CmdSuccess(count) => CommandResult.successCount(count)
      case CmdError(msg)     => throw new CommandException(Text.of(msg))
      case CmdSyntaxError(msg, pos) =>
        val e =
          if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
          else new CommandException(Text.of(msg))
        throw e
      case CmdUsageError(msg, pos) =>
        //TODO: Custom exception
        val e =
          if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
          else new CommandException(Text.of(msg))
        throw e
      case e: MultipleCmdErrors => throw new CommandException(Text.of(e.msg)) //TODO: Better error here
    }
  }

  override def getSuggestions(
      source: CommandSource,
      arguments: String,
      targetPosition: Location[World]
  ): util.List[String] =
    command.userValidator
      .validate(source)
      .map(source => command.suggestions(source, targetPosition, ScammanderHelper.stringToRawArgs(arguments)))
      .getOrElse(Nil)
      .asJava

  override def testPermission(source: CommandSource): Boolean = source.hasPermission(extra.permission)

  override def getShortDescription(source: CommandSource): Optional[Text] = extra.shortDescription(source) match {
    case Some(description) => Optional.of(description)
    case None              => Optional.empty()
  }

  override def getHelp(source: CommandSource): Optional[Text] = extra.help(source) match {
    case Some(help) => Optional.of(help)
    case None       => Optional.empty()
  }

  override def getUsage(source: CommandSource): Text = Text.of(command.usage(source))

  def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
    val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
    if (res.isPresent) Some(res.get()) else None
  }
}
