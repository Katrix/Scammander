package net.katsstuff.scammander.bukkit.components

import cats.MonadError

trait BukkitBaseAllEither extends BukkitBaseAll {

  type G[A] = Either[CommandFailureNEL, A]

  implicit protected def G: MonadError[Either[CommandFailureNEL, ?], CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither

  override protected def runG[A](computation: Either[CommandFailureNEL, A]): Either[CommandFailureNEL, A] = computation
}
