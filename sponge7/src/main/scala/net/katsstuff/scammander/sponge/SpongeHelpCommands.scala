package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.service.pagination.PaginationList
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.action.TextActions
import org.spongepowered.api.text.format.{TextColors, TextStyles}
import org.spongepowered.api.world.{Location, World}

import net.katsstuff.scammander.{HelpCommands, HelperParameters, NormalParameters, ScammanderBase}

trait SpongeHelpCommands extends HelpCommands[CommandSource, Unit, Location[World]] {
  self: ScammanderBase[CommandSource, Unit, Location[World]]
    with NormalParameters[CommandSource, Unit, Location[World]]
    with HelperParameters[CommandSource, Unit, Location[World]]
    with SpongeBase =>

  override type Title = Text

  override def sendMultipleCommandHelp(
      title: Text,
      source: CommandSource,
      commands: Set[ChildCommand[_, _]]
  ): CommandStep[CommandSuccess] = {
    val pages = PaginationList.builder()
    pages.title(Text.of(TextColors.RED, title))

    val helpTexts = commands.toSeq.filter(_.command.testPermission(source)).sortBy(_.aliases.head).map { child =>
      createCommandHelp(source, child.aliases.mkString("|"), child.command, detail = false)
    }

    pages.contents(helpTexts: _*)
    pages.sendTo(source)

    Command.successStep()
  }

  override def sendCommandHelp(
      title: Text,
      source: CommandSource,
      command: StaticChildCommand[_, _],
      path: List[String]
  ): CommandStep[CommandSuccess] = {
    if (command.testPermission(source)) {
      val commandName = path.mkString("/", " ", "")
      source.sendMessage(createCommandHelp(source, commandName, command, detail = true))
      Command.successStep()
    } else Command.errorStep("You don't have the permission to see the help for this command")
  }

  def createCommandHelp(
      source: CommandSource,
      commandName: String,
      command: StaticChildCommand[_, _],
      detail: Boolean
  ): Text = {
    val commandUsage = command.getUsage(source)

    val helpBuilder =
      Text.builder().append(Text.of(TextColors.GREEN, TextStyles.UNDERLINE, commandName, " ", commandUsage))

    val commandHelp        = command.info.help(source)
    val commandDescription = command.info.shortDescription(source)

    helpBuilder.onClick(TextActions.suggestCommand(commandName))

    if (detail) {
      commandHelp.orElse(commandDescription).foreach(t => helpBuilder.append(Text.of(" - ", t)))
    } else {
      commandDescription.foreach(t => helpBuilder.append(Text.of(" - ", t)))
    }

    commandDescription.foreach(t => helpBuilder.onHover(TextActions.showText(t)))

    helpBuilder.build()
  }
}
