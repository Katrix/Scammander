package net.katsstuff.scammander.sponge

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.util.blockray.{BlockRay, BlockRayHit}
import org.spongepowered.api.world.{Location, World}
import scala.collection.JavaConverters._

import com.flowpowered.math.vector.{Vector3d, Vector3i}

import cats.data.{NonEmptyList, StateT}
import net.katsstuff.scammander.{OrParameters, ScammanderBase, ScammanderHelper}
import net.katsstuff.scammander
import shapeless._

trait SpongeOrParameter {
  self: ScammanderBase[
    ({ type L[A] = Either[NonEmptyList[scammander.CommandFailure], A] })#L,
    CommandSource,
    Unit,
    Location[World]
  ] with OrParameters[
      ({ type L[A] = Either[NonEmptyList[scammander.CommandFailure], A] })#L,
      CommandSource,
      Unit,
      Location[World]
    ]
    with SpongeValidators =>

  type CommandStep[A] = Either[NonEmptyList[CommandFailure], A]

  /**
    * A typeclass which returns what the sender is currently targeting.
    */
  trait Targeter[A] {
    def getTarget(source: CommandSource, pos: Int): CommandStep[A]
  }
  object Targeter {
    //noinspection ConvertExpressionToSAM
    def instance[A](f: (CommandSource, Int) => CommandStep[A]): Targeter[A] = new Targeter[A] {
      override def getTarget(source: CommandSource, pos: Int): CommandStep[A] = f(source, pos)
    }

    implicit def entityTargeter[A <: Entity](implicit typeable: Typeable[A]): Targeter[A] = new Targeter[A] {
      private val EntityCase = TypeCase[A]

      override def getTarget(source: CommandSource, pos: Int): CommandStep[A] = {

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

          target.toRight(Command.usageErrorNel("Not looking at an entity", pos))
        }
      }
    }

    implicit def blockHitTargeter: Targeter[BlockRayHit[World]] =
      Targeter.instance { (source: CommandSource, pos: Int) =>
        entitySender[Entity].validate(source).flatMap { entity =>
          val res = BlockRay.from(entity).distanceLimit(10).build().asScala.toStream.headOption
          res.toRight(Command.usageErrorNel("Not looking at an block", pos))
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

    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], OrTarget[Base]] =
      for {
        pos <- ScammanderHelper.getPos
        res <- ScammanderHelper.withFallback(
          parameter.parse(source, extra),
          Command.liftFStateParse(targeter.getTarget(source, pos))
        )
      } yield Or(res)

    override def usage(source: CommandSource): CommandStep[String] = {
      import cats.syntax.either._
      targeter.getTarget(source, -1).map(_ => s"[$name]").orElse(super.usage(source))
    }
  }
}
