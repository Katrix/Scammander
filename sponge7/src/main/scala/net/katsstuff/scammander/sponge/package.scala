package net.katsstuff.scammander

import org.spongepowered.api.command.CommandSource

package object sponge extends ScammanderUniverse[CommandSource, Unit] {
  override def hasSenderPermission(sender: CommandSource, permission: String): Boolean =
    sender.hasPermission(permission)
}
