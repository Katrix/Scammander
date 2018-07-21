package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds
import scala.collection.JavaConverters._

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.util.blockray.{BlockRay, BlockRayHit}
import org.spongepowered.api.world.{Location, World}

import com.flowpowered.math.vector.{Vector3d, Vector3i}

import cats.syntax.all._
import net.katsstuff.scammander.{OrParameters, ScammanderHelper}
import shapeless._

trait SpongeOrParameter[F[_]] {
  self: SpongeBase[F]
    with OrParameters[F]
    with SpongeValidators[F] =>

  /**
    * A typeclass which returns what the sender is currently targeting.
    */
  trait Targeter[A] {
    def getTarget(source: CommandSource, pos: Int): F[A]
  }
  object Targeter {
    //noinspection ConvertExpressionToSAM
    def instance[A](f: (CommandSource, Int) => F[A]): Targeter[A] = new Targeter[A] {
      override def getTarget(source: CommandSource, pos: Int): F[A] = f(source, pos)
    }

    implicit def entityTargeter[A <: Entity](implicit typeable: Typeable[A]): Targeter[A] = new Targeter[A] {
      private val EntityCase = TypeCase[A]

      override def getTarget(source: CommandSource, pos: Int): F[A] = {

        entitySender[Entity].validate(source).flatMap { entity =>
          val target = entity.getWorld
            .getIntersectingEntities(entity, 10)
            .asScala
            .collect {
              case hit if hit.getEntity != entity => hit.getEntity
            }
            .collectFirst {
              case EntityCase(e) => e
            }

          F.fromOption(target, Command.usageErrorNel("Not looking at an entity", pos))
        }
      }
    }

    implicit def blockHitTargeter: Targeter[BlockRayHit[World]] =
      Targeter.instance { (source: CommandSource, pos: Int) =>
        entitySender[Entity].validate(source).flatMap { entity =>
          val res = BlockRay.from(entity).distanceLimit(10).build().asScala.toStream.headOption
          F.fromOption(res, Command.usageErrorNel("Not looking at an block", pos))
        }
      }

    implicit def locationTargeter: Targeter[Location[World]] =
      Targeter.instance { (source: CommandSource, pos: Int) =>
        blockHitTargeter.getTarget(source, pos).map(_.getLocation)
      }

    implicit def vector3iTargeter: Targeter[Vector3i] =
      Targeter.instance { (source: CommandSource, pos: Int) =>
        blockHitTargeter.getTarget(source, pos).map(_.getBlockPosition)
      }

    implicit def vector3dTargeter: Targeter[Vector3d] =
      Targeter.instance { (source: CommandSource, pos: Int) =>
        blockHitTargeter.getTarget(source, pos).map(_.getPosition)
      }
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

    override def parse(source: CommandSource, extra: Unit): Parser[OrTarget[Base]] =
      for {
        pos <- ScammanderHelper.getPos
        res <- ScammanderHelper
          .withFallbackParser(parameter.parse(source, extra), Command.liftFtoParser(targeter.getTarget(source, pos)))
      } yield Or(res)

    override def usage(source: CommandSource): F[String] = {
      targeter.getTarget(source, -1).map(_ => s"[$name]").handleErrorWith(_ => super.usage(source))
    }
  }
}
