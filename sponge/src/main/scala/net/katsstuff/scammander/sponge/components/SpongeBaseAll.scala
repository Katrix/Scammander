package net.katsstuff.scammander.sponge.components

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.world.{Location, World}

import cats.MonadError
import cats.data.NonEmptyList
import net.katsstuff.scammander.{CommandFailure, ScammanderBaseAll}

trait SpongeBaseAll[F[_]]
    extends SpongeBase[F]
    with ScammanderBaseAll[F, CommandSource, Unit, Option[Location[World]]]
    with SpongeValidators[F]
    with SpongeParameter[F]
    with SpongeOrParameter[F]
    with SpongeHelpCommands[F]
