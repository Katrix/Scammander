package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.service.pagination.PaginationList
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.action.TextActions
import org.spongepowered.api.text.format.TextColors._
import org.spongepowered.api.text.format.TextStyles._
import org.spongepowered.api.world.{Location, World}
import net.katsstuff.scammander.{HelpCommands, HelperParameters, NormalParameters, ScammanderBase}

trait SpongeHelpCommands extends HelpCommands[CommandSource, Unit, Location[World]] {
  self: ScammanderBase[CommandSource, Unit, Location[World]]
    with NormalParameters[CommandSource, Unit, Location[World]]
    with HelperParameters[CommandSource, Unit, Location[World]]
    with SpongeBase =>

  override type Title = Text

  private val Branch = "├─"
  private val Line   = "│"
  private val End    = "└─"

  override def sendMultipleCommandHelp(
      title: Text,
      source: CommandSource,
      commands: Set[ChildCommand[_, _]]
  ): CommandStep[CommandSuccess] = {
    val pages = PaginationList.builder()
    pages.title(title)

    val helpTexts = commands.toSeq
      .filter(_.command.testPermission(source))
      .sortBy(_.aliases.head)
      .flatMap { child =>
        createCommandHelp(
          source,
          child.aliases.mkString("/", "|", ""),
          s"/${child.aliases.head}",
          child.command,
          detail = false
        )
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
      val pages       = PaginationList.builder()
      pages.title(title)
      pages.contents(createCommandHelp(source, commandName, commandName, command, detail = true): _*)
      pages.sendTo(source)

      Command.successStep()
    } else Command.errorStep("You don't have the permission to see the help for this command")
  }

  def createCommandHelp(
      source: CommandSource,
      commandName: String,
      fullCommandName: String,
      command: StaticChildCommand[_, _],
      detail: Boolean,
      indent: Int = 0,
      isIndentEnd: Boolean = false
  ): Seq[Text] = {
    val usage = command.getUsage(source)

    val helpBuilder = Text.builder().append(Text.of(GREEN, UNDERLINE, commandName, " ", usage)).onClick(TextActions
      .suggestCommand(fullCommandName))

    val commandHelp        = command.info.help(source)
    val commandDescription = command.info.shortDescription(source)

    commandDescription.foreach(desc => helpBuilder.onHover(TextActions.showText(desc)))

    val hoverOpt = if(detail) commandHelp.orElse(commandDescription) else commandDescription
    hoverOpt.foreach(text => helpBuilder.append(Text.of(" - ", text)))

    val children = command.command.children.toSeq.sortBy(_.aliases.head)
    val childHelp = if (children.nonEmpty) {

      val childrenTopHelp = children.init.flatMap {
        case ChildCommand(aliases, childCommand) =>
          createCommandHelp(
            source,
            aliases.mkString("|"),
            s"$fullCommandName ${aliases.head}",
            childCommand,
            detail = false,
            indent = indent + 1
          )
      }
      val lastChild = children.last
      val lastChildHelp = createCommandHelp(
        source,
        lastChild.aliases.mkString("|"),
        s"$fullCommandName ${lastChild.aliases.head}",
        lastChild.command,
        detail = false,
        indent = indent + 1,
        isIndentEnd = true
      )
      childrenTopHelp ++ lastChildHelp
    } else Nil

    if (indent == 1) {
      val piece    = if (isIndentEnd) End else Branch
      val indented = Text.of(piece, helpBuilder)

      indented +: childHelp
    } else if (indent != 0) {
      val end      = if (isIndentEnd) End else Branch
      val spaces   = (indent - 1) * 2
      val space    = " " * spaces
      val indented = Text.of(Line, space, end, " ", helpBuilder)

      indented +: childHelp
    } else helpBuilder.build() +: childHelp
  }
}
