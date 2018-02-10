package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.world.{Location, World}

import net.katsstuff.scammander.ScammanderBaseAll

trait SpongeBaseAll
    extends SpongeBase
    with ScammanderBaseAll[CommandSource, Unit, Location[World]]
    with SpongeValidators
    with SpongeParameter
    with SpongeOrParameter
