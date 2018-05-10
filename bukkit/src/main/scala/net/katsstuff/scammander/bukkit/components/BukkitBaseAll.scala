package net.katsstuff.scammander.bukkit.components

import org.bukkit.command.CommandSender

import cats.MonadError
import cats.data.NonEmptyList
import net.katsstuff.scammander.{CommandFailure, ScammanderBaseAll}

trait BukkitBaseAll
    extends BukkitBase[Either[NonEmptyList[CommandFailure], ?]]
    with ScammanderBaseAll[Either[NonEmptyList[CommandFailure], ?], CommandSender, BukkitExtra, BukkitExtra]
    with BukkitValidators[Either[NonEmptyList[CommandFailure], ?]]
    with BukkitParameters[Either[NonEmptyList[CommandFailure], ?]] {

  override protected def runComputation[A](computation: Either[CommandFailureNEL, A]): Either[CommandFailureNEL, A] =
    computation

  implicit override def F: MonadError[Either[CommandFailureNEL, ?], CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither
}
