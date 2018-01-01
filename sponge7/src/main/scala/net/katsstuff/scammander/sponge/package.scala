package net.katsstuff.scammander

import org.spongepowered.api.command.{CommandMapping, CommandSource}
import org.spongepowered.api.entity.living.player.Player

package object sponge extends ScammanderUniverse[CommandSource, Unit] {
  override def hasSenderPermission(sender: CommandSource, permission: String): Boolean =
    sender.hasPermission(permission)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) extends AnyVal {
    def toSponge(extra: SpongeCommandExtra): SpongeCommand[Sender, Param] = SpongeCommand(command, extra)

    def register(plugin: AnyRef, extra: SpongeCommandExtra, aliases: Seq[String]): Option[CommandMapping] =
      toSponge(extra).register(plugin, aliases)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player => Right(player)
    case _              => Left(CmdError(""))
  }(identity)

  implicit val commandSourceSender: UserValidator[CommandSource] =
    UserValidator.mkTransformer(Right.apply)(identity)
}
