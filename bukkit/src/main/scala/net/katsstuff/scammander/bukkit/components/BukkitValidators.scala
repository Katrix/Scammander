package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import java.net.InetAddress

import cats.syntax.all._
import net.katsstuff.scammander.ScammanderTypes
import org.bukkit.command.{BlockCommandSender, CommandSender, ProxiedCommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Location, OfflinePlayer}
import shapeless.{TypeCase, Typeable}

trait BukkitValidators { self: BukkitBase =>

  implicit val playerSender: UserValidator[Player] = new UserValidator[Player] {
    override def validate[F[_]: ParserError](sender: CommandSender): F[Player] = sender match {
      case player: Player => player.pure[F]
      case _              => Result.usageErrorF("This command can only be used by players", -1)
    }
  }

  implicit val offlinePlayerSender: UserValidator[OfflinePlayer] = new UserValidator[OfflinePlayer] {
    override def validate[F[_]: ScammanderTypes.ParserError](sender: CommandSender): F[OfflinePlayer] = sender match {
      case player: OfflinePlayer => (player: OfflinePlayer).pure[F]
      case _                     => Result.usageErrorF("This command can only be used by players", -1)
    }
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    new UserValidator[A] {
      override def validate[F[_]: ParserError](sender: CommandSender): F[A] = sender match {
        case EntityCase(entity)          => entity.pure[F]
        case proxy: ProxiedCommandSender => entitySender[A].validate(proxy.getCaller)
        case _                           => Result.usageErrorF("This command can only be used by players", -1)
      }
    }
  }

  implicit val locationSender: UserValidator[Location] = new UserValidator[Location] {
    override def validate[F[_]: ParserError](sender: CommandSender): F[Location] = sender match {
      case entity: Entity                  => entity.getLocation.pure[F]
      case blockSender: BlockCommandSender => blockSender.getBlock.getLocation.pure[F]
      case proxy: ProxiedCommandSender     => locationSender.validate(proxy.getCaller)
      case _                               => Result.usageErrorF("This command can only be used by things which have a location", -1)
    }
  }

  implicit val vector3dSender: UserValidator[BukkitVector] = locationSender.map(_.toVector)

  implicit val ipSender: UserValidator[InetAddress] = new UserValidator[InetAddress] {
    override def validate[F[_]: ParserError](sender: CommandSender): F[InetAddress] = sender match {
      case player: Player              => player.getAddress.getAddress.pure[F]
      case proxy: ProxiedCommandSender => ipSender.validate(proxy.getCaller)
      case _                           => Result.usageErrorF("This command can only be used by things which have an IP", -1)
    }
  }
}
