package net.katsstuff.scammander.sponge

import java.net.InetAddress

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.command.source.{ProxySource, RemoteSource}
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.world.{Locatable, Location, World}

import com.flowpowered.math.vector.Vector3d

import cats.data.NonEmptyList
import net.katsstuff.scammander.ScammanderBase
import net.katsstuff.scammander
import shapeless._

trait SpongeValidators {
  self: ScammanderBase[({type L[A] = Either[NonEmptyList[scammander.CommandFailure], A]})#L, CommandSource, Unit, Location[World]] =>

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player     => Right(player)
    case proxy: ProxySource => playerSender.validate(proxy.getOriginalSource)
    case _                  => Left(NonEmptyList.one(Command.usageError("This command can only be used by players", -1)))
  }

  implicit val userSender: UserValidator[User] = UserValidator.mkValidator {
    case user: User         => Right(user)
    case proxy: ProxySource => userSender.validate(proxy.getOriginalSource)
    case _                  => Left(NonEmptyList.one(Command.usageError("This command can only be used by users", -1)))
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity) => Right(entity)
      case proxy: ProxySource => entitySender.validate(proxy.getOriginalSource)
      case _                  => Left(NonEmptyList.one(Command.usageError("This command can only be used by players", -1)))
    }
  }

  implicit val locationSender: UserValidator[Location[World]] = UserValidator.mkValidator {
    case locatable: Locatable => Right(locatable.getLocation)
    case proxy: ProxySource   => locationSender.validate(proxy.getOriginalSource)
    case _                    => Left(NonEmptyList.one(Command.usageError("This command can only be used by players", -1)))
  }

  implicit val vector3dSender: UserValidator[Vector3d] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.getPosition))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case remote: RemoteSource => Right(remote.getConnection.getAddress.getAddress)
    case proxy: ProxySource   => ipSender.validate(proxy.getOriginalSource)
    case _                    => Left(NonEmptyList.one(Command.usageError("This command can only be used by things which have an IP", -1)))
  }
}
