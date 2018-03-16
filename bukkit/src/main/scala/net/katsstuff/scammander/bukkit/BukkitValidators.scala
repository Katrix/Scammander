package net.katsstuff.scammander.bukkit

import java.net.InetAddress

import org.bukkit.{Location, OfflinePlayer}
import org.bukkit.command.{BlockCommandSender, CommandSender, ProxiedCommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.util.{Vector => BukkitVector}

import net.katsstuff.scammander.CrossCompatibility._
import net.katsstuff.scammander.ScammanderBase
import shapeless.{TypeCase, Typeable}

trait BukkitValidators {
  self: ScammanderBase[CommandSender, BukkitExtra, BukkitExtra] =>

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player => Right(player)
    case _              => Left(CommandUsageError("This command can only be used by players", -1))
  }

  implicit val offlinePlayerSender: UserValidator[OfflinePlayer] = UserValidator.mkValidator {
    case player: OfflinePlayer => Right(player)
    case _                     => Left(CommandUsageError("This command can only be used by players", -1))
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity)          => Right(entity)
      case proxy: ProxiedCommandSender => entitySender.validate(proxy.getCaller)
      case _                           => Left(CommandUsageError("This command can only be used by players", -1))
    }
  }

  implicit val locationSender: UserValidator[Location] = UserValidator.mkValidator {
    case entity: Entity                  => Right(entity.getLocation)
    case blockSender: BlockCommandSender => Right(blockSender.getBlock.getLocation)
    case proxy: ProxiedCommandSender     => locationSender.validate(proxy.getCaller)
    case _                               => Left(CommandUsageError("This command can only be used by things which have a location", -1))
  }

  implicit val vector3dSender: UserValidator[BukkitVector] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.toVector))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case player: Player              => Right(player.getAddress.getAddress)
    case proxy: ProxiedCommandSender => ipSender.validate(proxy.getCaller)
    case _                           => Left(CommandUsageError("This command can only be used by things which have an IP", -1))
  }
}
