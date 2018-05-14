package net.katsstuff.scammander.sponge.components

import cats.MonadError
import cats.data.NonEmptyList
import net.katsstuff.scammander.CommandFailure

trait SpongeBaseAllEither extends SpongeBaseAll[({ type L[A] = Either[NonEmptyList[CommandFailure], A] })#L] {

  override protected def runComputation[A](computation: Either[CommandFailureNEL, A]): Either[CommandFailureNEL, A] =
    computation

  implicit override def F: MonadError[({ type L[A] = Either[CommandFailureNEL, A] })#L, CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither
}
