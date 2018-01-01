package net.katsstuff.scammander.sponge

import java.util
import java.util.Optional

import scala.collection.JavaConverters._

import org.spongepowered.api.Sponge
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.command.{CommandCallable, CommandException, CommandMapping, CommandResult, CommandSource}
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

case class SpongeCommand[Sender, Param](command: Command[Sender, Param], extra: SpongeCommandExtra)
    extends CommandCallable {

  override def process(source: CommandSource, arguments: String): CommandResult = {
    val res = for {
      sender <- command.userValidator.validate(source)
      param  <- command.par.parse(source, (), arguments.split(" ").toList)
    } yield command.run(sender, (), param._2)

    res.merge match {
      case CmdSuccess(count)    => CommandResult.successCount(count)
      case CmdError(msg)        => throw new CommandException(Text.of(msg))
      case CmdSyntaxError(msg)  => throw new ArgumentParseException(Text.of(msg), arguments, ???)
      case CmdUsageError(msg)   => throw new ArgumentParseException(Text.of(msg), arguments, ???) //TODO: Custom exception
      case e: MultipleCmdErrors => throw new CommandException(Text.of(e.msg))
    }
  }

  override def getSuggestions(
      source: CommandSource,
      arguments: String,
      targetPosition: Location[World]
  ): util.List[String] =
    command.userValidator
      .validate(source)
      .map(command.suggestions(_, arguments.split(" ").toList))
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
