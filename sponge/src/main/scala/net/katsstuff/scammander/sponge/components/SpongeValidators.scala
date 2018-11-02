package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds

import java.net.InetAddress

import cats.syntax.all._
import com.flowpowered.math.vector.Vector3d
import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.command.source.{ProxySource, RemoteSource}
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.world.{Locatable, Location, World}
import shapeless._

trait SpongeValidators { self: SpongeBase =>

  implicit val playerSender: UserValidator[Player] = new UserValidator[Player] {
    override def validate[F[_]: ParserError](sender: CommandSource): F[Player] = sender match {
      case player: Player     => player.pure[F]
      case proxy: ProxySource => playerSender.validate[F](proxy.getOriginalSource)
      case _                  => Result.usageErrorF[F, Player]("This command can only be used by players", -1)
    }
  }

  implicit val userSender: UserValidator[User] = new UserValidator[User] {
    override def validate[F[_]: ParserError](sender: CommandSource): F[User] = sender match {
      case user: User         => (user: User).pure[F]
      case proxy: ProxySource => userSender.validate[F](proxy.getOriginalSource)
      case _                  => Result.usageErrorF[F, User]("This command can only be used by users", -1)
    }
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    new UserValidator[A] {
      override def validate[F[_]: ParserError](sender: CommandSource): F[A] = sender match {
        case EntityCase(entity) => entity.pure[F]
        case proxy: ProxySource => entitySender[A].validate[F](proxy.getOriginalSource)
        case _                  => Result.usageErrorF[F, A]("This command can only be used by players", -1)
      }
    }
  }

  implicit val locationSender: UserValidator[Location[World]] = new UserValidator[Location[World]] {
    override def validate[F[_]: ParserError](sender: CommandSource): F[Location[World]] = sender match {
      case locatable: Locatable => locatable.getLocation.pure[F]
      case proxy: ProxySource   => locationSender.validate[F](proxy.getOriginalSource)
      case _                    => Result.usageErrorF[F, Location[World]]("This command can only be used by players", -1)
    }
  }

  implicit val vector3dSender: UserValidator[Vector3d] = locationSender.map(_.getPosition)

  implicit val ipSender: UserValidator[InetAddress] = new UserValidator[InetAddress] {
    override def validate[F[_]: ParserError](sender: CommandSource): F[InetAddress] = sender match {
      case remote: RemoteSource => remote.getConnection.getAddress.getAddress.pure[F]
      case proxy: ProxySource   => ipSender.validate[F](proxy.getOriginalSource)
      case _                    => Result.usageErrorF[F, InetAddress]("This command can only be used by things which have an IP", -1)
    }
  }
}
