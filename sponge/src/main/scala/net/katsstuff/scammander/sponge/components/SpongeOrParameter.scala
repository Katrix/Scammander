package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds

import scala.collection.JavaConverters._

import cats.Functor
import cats.syntax.all._
import com.flowpowered.math.vector.{Vector3d, Vector3i}
import net.katsstuff.scammander.{OrParameters, ScammanderHelper}
import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.util.blockray.{BlockRay, BlockRayHit}
import org.spongepowered.api.world.{Location, World}
import shapeless._

trait SpongeOrParameter {
  self: SpongeBase with OrParameters with SpongeValidators =>

  /**
    * A typeclass which returns what the sender is currently targeting.
    */
  trait Targeter[A] {
    def getTarget[F[_]: ParserError](source: CommandSource, pos: Int): F[A]
  }
  object Targeter {

    implicit val functor: Functor[Targeter] = new Functor[Targeter] {
      override def map[A, B](fa: Targeter[A])(f: A => B): Targeter[B] = new Targeter[B] {
        override def getTarget[F[_]: ParserError](source: CommandSource, pos: Int): F[B] =
          fa.getTarget[F](source, pos).map(f)
      }
    }

    implicit def entityTargeter[A <: Entity](implicit typeable: Typeable[A]): Targeter[A] = new Targeter[A] {
      private val EntityCase = TypeCase[A]

      override def getTarget[F[_]](source: CommandSource, pos: Int)(implicit E: ParserError[F]): F[A] = {

        entitySender[Entity].validate[F](source).flatMap { entity =>
          val target = entity.getWorld
            .getIntersectingEntities(entity, 10)
            .asScala
            .collect {
              case hit if hit.getEntity != entity => hit.getEntity
            }
            .collectFirst {
              case EntityCase(e) => e
            }

          E.fromOption(target, Result.usageErrorNel("Not looking at an entity", pos))
        }
      }
    }

    implicit val blockHitTargeter: Targeter[BlockRayHit[World]] = {
      new Targeter[BlockRayHit[World]] {
        override def getTarget[F[_]](source: CommandSource, pos: Int)(
            implicit E: ParserError[F]
        ): F[BlockRayHit[World]] =
          entitySender[Entity].validate[F](source).flatMap { entity =>
            val res = BlockRay.from(entity).distanceLimit(10).build().asScala.toStream.headOption
            E.fromOption(res, Result.usageErrorNel("Not looking at an block", pos))
          }
      }
    }

    implicit val locationTargeter: Targeter[Location[World]] = blockHitTargeter.map(_.getLocation)

    implicit val vector3iTargeter: Targeter[Vector3i] = blockHitTargeter.map(_.getBlockPosition)

    implicit val vector3dTargeter: Targeter[Vector3d] = blockHitTargeter.map(_.getPosition)
  }

  /**
    * Used with [[Or]] to get what the sender is targeting.
    */
  sealed trait Target
  type OrTarget[Base] = Base Or Target
  implicit def orTargetParam[Base](
      implicit parameter: Parameter[Base],
      targeter: Targeter[Base]
  ): Parameter[OrTarget[Base]] = new ProxyParameter[OrTarget[Base], Base] {
    override def param: Parameter[Base] = parameter

    override def parse[F[_]: ParserState: ParserError](source: CommandSource, extra: Unit): F[OrTarget[Base]] =
      for {
        pos <- ScammanderHelper.getPos
        res <- ScammanderHelper.withFallback(parameter.parse(source, extra), targeter.getTarget[F](source, pos))
      } yield Or(res)

    override def usage[F[_]: ParserError](source: CommandSource): F[String] =
      targeter.getTarget[F](source, -1).map(_ => s"[$name]").handleErrorWith(_ => super.usage[F](source))
  }
}
