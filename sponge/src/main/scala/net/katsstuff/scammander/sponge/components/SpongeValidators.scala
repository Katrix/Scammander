package net.katsstuff.scammander.sponge.components

import java.net.InetAddress

import scala.language.higherKinds

import org.spongepowered.api.command.source.{ProxySource, RemoteSource}
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.world.{Locatable, Location, World}

import com.flowpowered.math.vector.Vector3d

import cats.syntax.all._
import shapeless._

trait SpongeValidators[F[_]] { self: SpongeBase[F] =>

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player     => player.pure
    case proxy: ProxySource => playerSender.validate(proxy.getOriginalSource)
    case _                  => Command.usageErrorF("This command can only be used by players", -1)
  }

  implicit val userSender: UserValidator[User] = UserValidator.mkValidator {
    case user: User         => F.pure(user)
    case proxy: ProxySource => userSender.validate(proxy.getOriginalSource)
    case _                  => Command.usageErrorF("This command can only be used by users", -1)
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity) => entity.pure
      case proxy: ProxySource => entitySender.validate(proxy.getOriginalSource)
      case _                  => Command.usageErrorF("This command can only be used by players", -1)
    }
  }

  implicit val locationSender: UserValidator[Location[World]] = UserValidator.mkValidator {
    case locatable: Locatable => locatable.getLocation.pure
    case proxy: ProxySource   => locationSender.validate(proxy.getOriginalSource)
    case _                    => Command.usageErrorF("This command can only be used by players", -1)
  }

  implicit val vector3dSender: UserValidator[Vector3d] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.getPosition))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case remote: RemoteSource => remote.getConnection.getAddress.getAddress.pure
    case proxy: ProxySource   => ipSender.validate(proxy.getOriginalSource)
    case _                    => Command.usageErrorF("This command can only be used by things which have an IP", -1)
  }
}
