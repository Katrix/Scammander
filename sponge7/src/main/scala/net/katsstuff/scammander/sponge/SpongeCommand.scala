package net.katsstuff.scammander.sponge

import java.util
import java.util.Optional

import scala.collection.JavaConverters._

import org.spongepowered.api.Sponge
import org.spongepowered.api.command.{CommandCallable, CommandException, CommandMapping, CommandResult, CommandSource}
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

case class SpongeCommand[Sender, Param](command: Command[Sender, Param], extra: SpongeCommandExtra)
    extends CommandCallable {

  override def process(source: CommandSource, arguments: String): CommandResult = {
    val res = for {
      sender <- command.senderTransformer.validate(source)
      param  <- command.par.parse(source, (), arguments.split(" ").toList).left.map(CmdError)
    } yield command.run(sender, (), param._2)

    res.merge match {
      case CmdSuccess(count) => CommandResult.successCount(count)
      case CmdError(msg)     => throw new CommandException(Text.of(msg))
    }
  }

  override def getSuggestions(
      source: CommandSource,
      arguments: String,
      targetPosition: Location[World]
  ): util.List[String] = command.par.suggestions(source, arguments.split(" ").toList)._2.asJava

  override def testPermission(source: CommandSource): Boolean = source.hasPermission(extra.permission)

  override def getShortDescription(source: CommandSource): Optional[Text] = extra.shortDescription(source) match {
    case Some(description) => Optional.of(description)
    case None              => Optional.empty()
  }

  override def getHelp(source: CommandSource): Optional[Text] = extra.help(source) match {
    case Some(help) => Optional.of(help)
    case None       => Optional.empty()
  }

  override def getUsage(source: CommandSource): Text = Text.of(command.par.usage)

  def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
    val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
    if (res.isPresent) Some(res.get()) else None
  }
}
