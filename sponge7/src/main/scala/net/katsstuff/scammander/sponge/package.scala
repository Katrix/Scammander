package net.katsstuff.scammander

import org.spongepowered.api.command.{CommandMapping, CommandSource}
import org.spongepowered.api.entity.living.player.Player
import org.spongepowered.api.world.{Location, World}

package object sponge extends ScammanderUniverse[CommandSource, Unit, Location[World]] {
  override def hasSenderPermission(sender: CommandSource, permission: String): Boolean =
    sender.hasPermission(permission)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) extends AnyVal {
    def toSponge(extra: SpongeCommandInfo): SpongeCommandWrapper[Sender, Param] = SpongeCommandWrapper(command, extra)

    def register(plugin: AnyRef, extra: SpongeCommandInfo, aliases: Seq[String]): Option[CommandMapping] =
      toSponge(extra).register(plugin, aliases)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player => Right(player)
    case _              => Left(CmdUsageError("This command can only be used by players", -1))
  }(identity)

  implicit val commandSourceSender: UserValidator[CommandSource] =
    UserValidator.mkTransformer(Right.apply)(identity)
}
