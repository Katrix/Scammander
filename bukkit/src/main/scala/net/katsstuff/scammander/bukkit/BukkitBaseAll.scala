package net.katsstuff.scammander.bukkit

import org.bukkit.command.CommandSender

import cats.data.NonEmptyList
import net.katsstuff.scammander.ScammanderBaseAll
import net.katsstuff.scammander

trait BukkitBaseAll
    extends BukkitBase
    with ScammanderBaseAll[Either[NonEmptyList[scammander.CommandFailure], ?], CommandSender, BukkitExtra, BukkitExtra]
    with BukkitValidators
    with BukkitParameters {

  override type CommandStep[A] = Either[CommandFailureNEL, A]
}
