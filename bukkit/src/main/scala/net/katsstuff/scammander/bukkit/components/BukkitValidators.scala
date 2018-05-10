package net.katsstuff.scammander.bukkit.components

import java.net.InetAddress

import scala.language.higherKinds

import org.bukkit.command.{BlockCommandSender, ProxiedCommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Location, OfflinePlayer}

import cats.syntax.all._
import shapeless.{TypeCase, Typeable}

trait BukkitValidators[F[_]] {
  self: BukkitBase[F] =>

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player => player.pure
    case _              => Command.usageErrorF("This command can only be used by players", -1)
  }

  implicit val offlinePlayerSender: UserValidator[OfflinePlayer] = UserValidator.mkValidator {
    case player: OfflinePlayer => F.pure(player)
    case _                     => Command.usageErrorF("This command can only be used by players", -1)
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity)          => entity.pure
      case proxy: ProxiedCommandSender => entitySender.validate(proxy.getCaller)
      case _                           => Command.usageErrorF("This command can only be used by players", -1)
    }
  }

  implicit val locationSender: UserValidator[Location] = UserValidator.mkValidator {
    case entity: Entity                  => entity.getLocation.pure
    case blockSender: BlockCommandSender => blockSender.getBlock.getLocation.pure
    case proxy: ProxiedCommandSender     => locationSender.validate(proxy.getCaller)
    case _                               => Command.usageErrorF("This command can only be used by things which have a location", -1)
  }

  implicit val vector3dSender: UserValidator[BukkitVector] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.toVector))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case player: Player              => player.getAddress.getAddress.pure
    case proxy: ProxiedCommandSender => ipSender.validate(proxy.getCaller)
    case _                           => Command.usageErrorF("This command can only be used by things which have an IP", -1)
  }
}
