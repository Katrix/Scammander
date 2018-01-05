/*
 * This file is part of Scammander, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katsstuff.scammander.sponge

import java.net.{InetAddress, URL}
import java.time.{Duration, LocalDateTime}
import java.util
import java.util.{Optional, UUID}

import scala.collection.JavaConverters._

import org.spongepowered.api.{CatalogType, Sponge}
import org.spongepowered.api.command.args.{ArgumentParseException, GenericArguments}
import org.spongepowered.api.command.source.ProxySource
import org.spongepowered.api.command._
import org.spongepowered.api.command.{CommandResult => SpongeCommandResult}
import org.spongepowered.api.data.DataContainer
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.plugin.PluginContainer
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.selector.Selector
import org.spongepowered.api.util.blockray.{BlockRay, BlockRayHit}
import org.spongepowered.api.world.{Location, World}

import com.flowpowered.math.vector.{Vector3d, Vector3i}

import net.katsstuff.scammander.misc.RawCmdArg
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import shapeless._

trait SpongeUniverse extends ScammanderUniverse[CommandSource, Unit, Location[World]] {

  def optionalToOption[A](optional: Optional[A]): Option[A] = if (optional.isPresent) Some(optional.get()) else None

  //Helpers used when registering command

  object Alias {
    def apply(single: String):                     Seq[String] = Seq(single)
    def multiple(first: String, aliases: String*): Seq[String] = first +: aliases
  }

  object Permission {
    def apply(perm: String): Some[String] = Some(perm)
    val none:                None.type    = None
  }

  object Help {
    def apply(f: CommandSource => Text): CommandSource => Option[Text] = f andThen Some.apply
    def apply(text: Text):               CommandSource => Option[Text] = _ => Some(text)
    val none:                            CommandSource => None.type    = _ => None
  }

  object Description {
    def apply(f: CommandSource => Text): CommandSource => Option[Text] = f andThen Some.apply
    def apply(text: Text):               CommandSource => Option[Text] = _ => Some(text)
    val none:                            CommandSource => None.type    = _ => None
  }

  case class NeedPermission[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[A, A] {
    val perm: String = w.value

    override def parse(source: CommandSource, extra: Unit, xs: List[RawCmdArg]): CommandStep[(List[RawCmdArg], A)] =
      if (source.hasPermission(perm)) param.parse(source, extra, xs)
      else
        Left(
          CommandUsageError(
            "You do not have the permissions needed to use this parameter",
            xs.headOption.map(_.start).getOrElse(-1)
          )
        )

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) =
      if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else (xs.tail, Nil)
  }

  implicit val playerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {
    override def name: String = "player"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[Player])] = {
      if (xs.nonEmpty) {
        val head = xs.head.content

        //TODO: Should an empty check be done here
        val players = if (head.startsWith("@")) {
          Selector.parse(xs.head.content).resolve(source).asScala.collect {
            case player: Player => player
          }
        } else optionalToOption(Sponge.getServer.getPlayer(head)).toSet

        Right((xs.tail, players.toSet))
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = {
      val head = xs.head
      val choices =
        if (head.content.startsWith("@")) Selector.complete(head.content).asScala
        else Sponge.getServer.getOnlinePlayers.asScala.map(_.getName)

      ScammanderHelper.suggestions(xs, choices)
    }
  }

  implicit val onlyOnePlayer: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]

  implicit def entityParam[A <: Entity](implicit typeable: Typeable[A]): Parameter[Set[A]] = new Parameter[Set[A]] {
    override def name: String = "entity"

    private val EntityType: TypeCase[A] = TypeCase[A]

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[A])] = {
      if (xs.nonEmpty) {
        //TODO: Should an empty check be done here
        val entities = Selector.parse(xs.head.content).resolve(source).asScala.collect {
          case EntityType(entity) => entity
        }
        Right((xs.tail, entities.toSet))
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = ScammanderHelper.suggestions(xs, Selector.complete(xs.head.content).asScala)
  }

  implicit val userParam:                        Parameter[User]            = ???
  implicit val worldParam:                       Parameter[World]           = ???
  implicit val vector3dParam:                    Parameter[Vector3d]        = ???
  implicit val locationParam:                    Parameter[Location[World]] = ???
  implicit def catalogedParam[A <: CatalogType]: Parameter[A]               = ???
  implicit val pluginParam:                      Parameter[PluginContainer] = ???
  implicit val urlParam:                         Parameter[URL]             = ???
  implicit val ipParam:                          Parameter[InetAddress]     = ???
  implicit val bigDecimalParam:                  Parameter[BigDecimal]      = ???
  implicit val bigIntParam:                      Parameter[BigInt]          = ???
  implicit val dataContainerParam:               Parameter[DataContainer]   = ???
  implicit val uuidParam:                        Parameter[UUID]            = ???
  implicit val dateTimeParam:                    Parameter[LocalDateTime]   = ??? //OrNow
  implicit val durationParam:                    Parameter[Duration]        = ??? //OrNow

  //TODO: orSource, orTarget, text

  trait Targeter[A] {
    def getTarget(source: CommandSource, pos: Int): CommandStep[A]
  }
  object Targeter {
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

          target.toRight(Command.usageError("Not looking at an entity", pos))
        }
      }
    }

    implicit def blockHitTargeter: Targeter[BlockRayHit[World]] = (source: CommandSource, pos: Int) => {
      entitySender[Entity].validate(source).flatMap { entity =>
        val res = BlockRay.from(entity).distanceLimit(10).build().asScala.toStream.headOption

        res.toRight(Command.usageError("Not looking at an block", pos))
      }
    }

    implicit def locationTargeter: Targeter[Location[World]] = (source: CommandSource, pos: Int) => {
      blockHitTargeter.getTarget(source, pos).map(_.getLocation)
    }

    implicit def vector3iTargeter: Targeter[Vector3i] = (source: CommandSource, pos: Int) => {
      blockHitTargeter.getTarget(source, pos).map(_.getBlockPosition)
    }

    implicit def vector3dTargeter: Targeter[Vector3d] = (source: CommandSource, pos: Int) => {
      blockHitTargeter.getTarget(source, pos).map(_.getPosition)
    }
  }

  sealed trait Target
  type OrTarget[Base] = Base Or Target
  implicit def orTargetParam[Base](
      implicit parameter: Parameter[Base],
      targeter: Targeter[Base]
  ): Parameter[OrTarget[Base]] = new ProxyParameter[OrTarget[Base], Base] {
    override def param: Parameter[Base] = parameter

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], OrTarget[Base])] = {
      val ret = parameter.parse(source, extra, xs).left.flatMap { e1 =>
        val res = targeter.getTarget(source, xs.headOption.map(_.start).getOrElse(-1)).left.map { e2 =>
          e1.merge(e2)
        }

        res.map(xs -> _)
      }

      ret.map(t => t._1 -> Or(t._2))
    }
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toSponge(info: CommandInfo): SpongeCommandWrapper[Sender, Param] = SpongeCommandWrapper(command, info)

    def register(
        plugin: AnyRef,
        aliases: Seq[String],
        permission: Option[String] = None,
        help: CommandSource => Option[Text] = _ => None,
        shortDescription: CommandSource => Option[Text] = _ => None
    ): Option[CommandMapping] =
      toSponge(CommandInfo(permission, help, shortDescription)).register(plugin, aliases)
  }

  implicit val playerSender: UserValidator[Player] = UserValidator.mkTransformer {
    case player: Player     => Right(player)
    case proxy: ProxySource => playerSender.validate(proxy.getOriginalSource)
    case _                  => Left(CommandUsageError("This command can only be used by players", -1))
  }(identity)

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkTransformer {
      case EntityCase(entity) => Right(entity)
      case proxy: ProxySource => entitySender.validate(proxy.getOriginalSource)
      case _                  => Left(CommandUsageError("This command can only be used by players", -1))
    } {
      case source: CommandSource => source
      case _                     => throw new IllegalStateException("Tried to convert non sender to sender using UserValidator")
    }
  }

  case class SpongeCommandWrapper[Sender, Param](command: Command[Sender, Param], info: CommandInfo)
      extends CommandCallable {

    override def process(source: CommandSource, arguments: String): SpongeCommandResult = {
      val res = for {
        sender <- command.userValidator.validate(source)
        param  <- command.par.parse(source, (), ScammanderHelper.stringToRawArgs(arguments))
      } yield command.run(sender, (), param._2)

      res.merge match {
        case CommandSuccess(count) => SpongeCommandResult.successCount(count)
        case CommandError(msg)     => throw new CommandException(Text.of(msg))
        case CommandSyntaxError(msg, pos) =>
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case CommandUsageError(msg, pos) =>
          //TODO: Custom exception
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case e: MultipleCommandErrors => throw new CommandException(Text.of(e.msg)) //TODO: Better error here
      }
    }

    override def getSuggestions(
        source: CommandSource,
        arguments: String,
        targetPosition: Location[World]
    ): util.List[String] =
      command.userValidator
        .validate(source)
        .map(source => command.suggestions(source, targetPosition, ScammanderHelper.stringToRawArgs(arguments)))
        .getOrElse(Nil)
        .asJava

    override def testPermission(source: CommandSource): Boolean = info.permission.forall(source.hasPermission)

    override def getShortDescription(source: CommandSource): Optional[Text] = info.shortDescription(source) match {
      case Some(description) => Optional.of(description)
      case None              => Optional.empty()
    }

    override def getHelp(source: CommandSource): Optional[Text] = info.help(source) match {
      case Some(help) => Optional.of(help)
      case None       => Optional.empty()
    }

    override def getUsage(source: CommandSource): Text = Text.of(command.usage(source))

    def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
      val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
      if (res.isPresent) Some(res.get()) else None
    }
  }
}
