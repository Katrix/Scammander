package net.katsstuff.scammander

import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.plugin.java.JavaPlugin

package object bukkit extends ScammanderUniverse[CommandSender, BukkitExtra] {
  override def hasSenderPermission(sender: CommandSender, permission: String): Boolean =
    sender.hasPermission(permission)

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) extends AnyVal {
    def toBukkit: BukkitCommandWrapper[Sender, Param] = BukkitCommandWrapper(command)

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player => Right(player)
    case _              => Left(CmdUsageError("This command can only be used by players", -1))
  }(identity)

  implicit val commandSourceSender: UserValidator[CommandSender] =
    UserValidator.mkTransformer(Right.apply)(identity)
}
