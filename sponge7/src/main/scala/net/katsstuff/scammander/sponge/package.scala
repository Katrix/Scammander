package net.katsstuff.scammander

import org.spongepowered.api.command.{CommandMapping, CommandSource}
import org.spongepowered.api.entity.living.player.Player
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

package object sponge extends ScammanderUniverse[CommandSource, Unit, Location[World]] {
  override def hasSenderPermission(sender: CommandSource, permission: String): Boolean =
    sender.hasPermission(permission)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) extends AnyVal {
    def toSponge(info: CommandInfo): SpongeCommandWrapper[Sender, Param] = SpongeCommandWrapper(command, info)

    def register(
        plugin: AnyRef,
        aliases: Seq[String],
        permission: Option[String] = None,
        help: CommandSource => Option[Text] = _ => None,
        shortDescription: CommandSource => Option[Text] = _ => None
    ): Option[CommandMapping] =
      toSponge(CommandInfo(permission, help, shortDescription)).register(plugin, aliases)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player => Right(player)
    case _              => Left(CmdUsageError("This command can only be used by players", -1))
  }(identity)

  implicit val commandSourceSender: UserValidator[CommandSource] =
    UserValidator.mkTransformer(Right.apply)(identity)
}
