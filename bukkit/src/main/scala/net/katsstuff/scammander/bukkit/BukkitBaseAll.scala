package net.katsstuff.scammander.bukkit

import org.bukkit.command.CommandSender

import cats.{Id, Monad}
import cats.data.{EitherT, NonEmptyList}
import net.katsstuff.scammander
import net.katsstuff.scammander.ScammanderBaseAll

trait BukkitBaseAll
    extends BukkitBase[Id]
    with ScammanderBaseAll[EitherT[Id, NonEmptyList[scammander.CommandFailure], ?], CommandSender, BukkitExtra, BukkitExtra]
    with BukkitValidators[Id]
    with BukkitParameters[Id] {

  implicit override def G: Monad[Id] = cats.catsInstancesForId

  override protected def runComputation[A](
      computation: EitherT[Id, CommandFailureNEL, A]
  ): Either[CommandFailureNEL, A] = computation.value

  override type CommandStep[A] = EitherT[Id, CommandFailureNEL, A]
}
