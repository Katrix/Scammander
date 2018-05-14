package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import org.bukkit.command.CommandSender

import net.katsstuff.scammander.ScammanderBaseAll

trait BukkitBaseAll[F[_]]
    extends BukkitBase[F]
    with ScammanderBaseAll[F, CommandSender, BukkitExtra, BukkitExtra]
    with BukkitValidators[F]
    with BukkitParameters[F]
