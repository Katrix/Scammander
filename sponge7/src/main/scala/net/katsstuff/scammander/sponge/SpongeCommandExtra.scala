package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.text.Text

case class SpongeCommandExtra(
    permission: String,
    help: CommandSource => Option[Text] = _ => None,
    shortDescription: CommandSource => Option[Text] = _ => None
)
