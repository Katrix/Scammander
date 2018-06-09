package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import net.katsstuff.scammander.ScammanderBaseAll

trait BukkitBaseAll[F[_]]
    extends BukkitBase[F]
    with ScammanderBaseAll[F]
    with BukkitValidators[F]
    with BukkitParameters[F]
