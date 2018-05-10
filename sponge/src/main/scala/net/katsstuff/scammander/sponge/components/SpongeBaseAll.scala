package net.katsstuff.scammander.sponge.components

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.world.{Location, World}

import cats.MonadError
import cats.data.NonEmptyList
import net.katsstuff.scammander.{CommandFailure, ScammanderBaseAll}

trait SpongeBaseAll
    extends SpongeBase[({ type L[A] = Either[NonEmptyList[CommandFailure], A] })#L]
    with ScammanderBaseAll[
      ({ type L[A] = Either[NonEmptyList[CommandFailure], A] })#L,
      CommandSource,
      Unit,
      Location[World]
    ]
    with SpongeValidators[({ type L[A]   = Either[NonEmptyList[CommandFailure], A] })#L]
    with SpongeParameter[({ type L[A]    = Either[NonEmptyList[CommandFailure], A] })#L]
    with SpongeOrParameter[({ type L[A]  = Either[NonEmptyList[CommandFailure], A] })#L]
    with SpongeHelpCommands[({ type L[A] = Either[NonEmptyList[CommandFailure], A] })#L] {

  override protected def runComputation[A](computation: Either[CommandFailureNEL, A]): Either[CommandFailureNEL, A] =
    computation
  implicit override def F: MonadError[({ type L[A] = Either[CommandFailureNEL, A] })#L, CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither
}
