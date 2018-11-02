package net.katsstuff.scammander.sponge.components

import net.katsstuff.scammander.{HelpCommands, HelperParameters, NormalParameters}
import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.service.pagination.PaginationList
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.action.TextActions
import org.spongepowered.api.text.format.TextColors._
import org.spongepowered.api.text.format.TextStyles._

trait SpongeHelpCommands extends HelpCommands {
  self: SpongeBase with NormalParameters with HelperParameters =>

  override type Title = Text

  private val Branch = "├─"
  private val Line   = "│"
  private val End    = "└─"

  override def sendMultipleCommandHelp(
      title: Title,
      source: RootSender,
      commands: Set[ChildCommand]
  ): G[CommandSuccess] = {
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

    Result.successF()
  }

  override def sendCommandHelp(
      title: Title,
      source: RootSender,
      command: StaticChildCommand,
      path: List[String]
  ): G[CommandSuccess] = {
    if (command.testPermission(source)) {
      val commandName = path.mkString("/", " ", "")
      val pages       = PaginationList.builder()
      pages.title(title)
      pages.contents(createCommandHelp(source, commandName, commandName, command, detail = true): _*)
      pages.sendTo(source)

      Result.successF()
    } else Result.errorF[G, CommandSuccess]("You don't have the permission to see the help for this command")
  }

  def createCommandHelp(
      source: CommandSource,
      commandName: String,
      fullCommandName: String,
      command: StaticChildCommand,
      detail: Boolean,
      indent: Int = 0,
      isIndentEnd: Boolean = false
  ): Seq[Text] = {
    val usage = command.getUsage(source)

    val helpBuilder = Text
      .builder()
      .append(Text.of(GREEN, UNDERLINE, commandName, " ", usage))
      .onClick(
        TextActions
          .suggestCommand(fullCommandName)
      )

    val commandHelp = runG(command.info.help(source))
      .fold(_ => Some(Text.of("Error when getting help")), identity)
    val commandDescription = runG(command.info.shortDescription(source))
      .fold(_ => Some(Text.of("Error when getting description")), identity)

    commandDescription.foreach(desc => helpBuilder.onHover(TextActions.showText(desc)))

    val hoverOpt = if (detail) commandHelp.orElse(commandDescription) else commandDescription
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
