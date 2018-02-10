package net.katsstuff.scammander.bukkit

import org.bukkit.command.CommandSender

import net.katsstuff.scammander.ScammanderBaseAll

trait BukkitBaseAll
    extends BukkitBase
    with ScammanderBaseAll[CommandSender, BukkitExtra, BukkitExtra]
    with BukkitValidators
    with BukkitParameters
