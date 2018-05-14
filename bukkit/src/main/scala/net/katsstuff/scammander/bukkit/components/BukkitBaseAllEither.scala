package net.katsstuff.scammander.bukkit.components

import cats.MonadError
import cats.data.NonEmptyList
import net.katsstuff.scammander.CommandFailure

trait BukkitBaseAllEither extends BukkitBaseAll[Either[NonEmptyList[CommandFailure], ?]] {

  override protected def runComputation[A](computation: Either[CommandFailureNEL, A]): Either[CommandFailureNEL, A] =
    computation

  implicit override def F: MonadError[Either[CommandFailureNEL, ?], CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither
}
