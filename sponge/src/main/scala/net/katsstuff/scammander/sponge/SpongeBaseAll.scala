package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.world.{Location, World}

import cats.data.NonEmptyList
import net.katsstuff.scammander.ScammanderBaseAll
import net.katsstuff.scammander

trait SpongeBaseAll
    extends SpongeBase
    with ScammanderBaseAll[({type L[A] = Either[NonEmptyList[scammander.CommandFailure], A]})#L, CommandSource, Unit, Location[World]]
    with SpongeValidators
    with SpongeParameter
    with SpongeOrParameter
    with SpongeHelpCommands
