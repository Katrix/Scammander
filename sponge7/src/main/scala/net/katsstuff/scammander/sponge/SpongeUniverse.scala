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

import java.io.{BufferedReader, StringReader}
import java.net.{InetAddress, UnknownHostException}
import java.util
import java.util.{Locale, Optional}
import java.util.concurrent.Callable

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.util.Try

import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.command.source.{ProxySource, RemoteSource}
import org.spongepowered.api.command.{CommandResult => SpongeCommandResult, _}
import org.spongepowered.api.data.DataContainer
import org.spongepowered.api.data.persistence.DataTranslators
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.plugin.PluginContainer
import org.spongepowered.api.service.user.UserStorageService
import org.spongepowered.api.text.Text
import org.spongepowered.api.text.selector.Selector
import org.spongepowered.api.util.blockray.{BlockRay, BlockRayHit}
import org.spongepowered.api.world.storage.WorldProperties
import org.spongepowered.api.world.{Locatable, Location, World}
import org.spongepowered.api.{CatalogType, Sponge}

import com.flowpowered.math.vector.{Vector3d, Vector3i}

import net.katsstuff.scammander.misc.RawCmdArg
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import ninja.leaping.configurate.hocon.HoconConfigurationLoader
import shapeless._

trait SpongeUniverse extends ScammanderUniverse[CommandSource, Unit, Location[World]] {

  def optionalToOption[A](optional: Optional[A]): Option[A] = if (optional.isPresent) Some(optional.get()) else None

  //Helpers used when registering command

  object Alias {
    def apply(first: String, aliases: String*): Seq[String] = first +: aliases
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

  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {
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

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Player)] = param.parse(source, extra, xs).map(t => t._1 -> t._2.value)
  }

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

  implicit val userParam: Parameter[Set[User]] = new Parameter[Set[User]] {
    private val userStorage = Sponge.getServiceManager.provideUnchecked(classOf[UserStorageService])
    override def name: String = "user"
    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[User])] = {
      val players = allPlayerParam.parse(source, extra, xs)
      val users = {
        val users = userStorage.getAll.asScala
          .collect {
            case profile if profile.getName.isPresent =>
              val name = profile.getName.get().toLowerCase(Locale.ROOT)
              name -> name
          }
          .toMap
          .filterKeys(userStorage.get(_).isPresent) //filter and map here are lazy, so we only do as many lookups as needed
          .mapValues(userStorage.get(_).get())

        ScammanderHelper.parseMany(name, xs, users)
      }

      players.map(t => t._1 -> t._2.map(player => player: User)).left.flatMap { e1 =>
        users.left.map { e2 =>
          e1.merge(e2)
        }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) =
      ScammanderHelper.suggestions(xs, userStorage.getAll.asScala.collect {
        case profile if profile.getName.isPresent => profile.getName.get()
      })
  }

  implicit val worldParam: Parameter[Set[WorldProperties]] = new Parameter[Set[WorldProperties]] {
    override def name: String = "world"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[WorldProperties])] =
      ScammanderHelper.parseMany(
        name,
        xs,
        Sponge.getServer.getAllWorldProperties.asScala
          .map(obj => obj.getWorldName.toLowerCase(Locale.ROOT) -> obj)
          .toMap
      )

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) =
      ScammanderHelper.suggestions(xs, Sponge.getServer.getAllWorldProperties.asScala.map(_.getWorldName))
  }

  implicit val vector3dParam: Parameter[Vector3d] = new Parameter[Vector3d] {
    override def name: String = "vector3"
    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Vector3d)] = {
      val relative = source match {
        case locatable: Locatable => Some(locatable.getLocation)
        case _                    => None
      }

      for {
        tx <- parseRelativeDouble(source, xs, relative.map(_.getX))
        ty <- parseRelativeDouble(source, tx._1, relative.map(_.getY))
        tz <- parseRelativeDouble(source, ty._1, relative.map(_.getZ))
      } yield tz._1 -> Vector3d.from(tx._2, ty._2, tz._2)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = (xs.drop(3), Nil)

    private def parseRelativeDouble(
        source: CommandSource,
        xs: List[RawCmdArg],
        relativeToOpt: Option[Double]
    ): CommandStep[(List[RawCmdArg], Double)] = {
      val RawCmdArg(start, end, arg) = xs.head

      if (arg.startsWith("~")) {
        relativeToOpt
          .toRight(CommandUsageError("Relative position specified but source does not have a position", start))
          .flatMap { relativeTo =>
            val newArg = arg.substring(1)
            if (newArg.isEmpty) Right(xs.tail -> relativeTo)
            else {
              doubleParam.parse(source, (), RawCmdArg(start, end, newArg) :: xs.tail).map {
                case (ys, res) =>
                  ys -> (res + relativeTo)
              }
            }
          }
      } else {
        doubleParam.parse(source, (), xs)
      }
    }
  }

  implicit val locationParam: Parameter[Set[Location[World]]] = new Parameter[Set[Location[World]]] {
    private val oneWorldParam = Parameter[OnlyOne[WorldProperties]]
    override def name: String = "location"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[Location[World]])] = {
      xs.headOption
        .collect {
          case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
            val entities = Selector.parse(arg).resolve(source).asScala.toSet
            Right(xs.tail -> entities.map(_.getLocation))
        }
        .getOrElse {
          for {
            wt <- oneWorldParam.parse(source, extra, xs).map(t => t._1 -> t._2.value).left.flatMap { e1 =>
              locationSender.validate(source).map(pos => xs -> pos.getExtent.getProperties).left.map { e2 =>
                e1.merge(e2)
              }
            }
            vt <- vector3dParam.parse(source, extra, wt._1)
          } yield {
            val world = Sponge.getServer.getWorld(wt._2.getUniqueId).get()
            vt._1 -> Set(new Location[World](world, vt._2))
          }
        }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = {
      xs.headOption
        .collect {
          case RawCmdArg(_, _, arg) if arg.startsWith("@") => xs.tail -> Selector.complete(arg).asScala
        }
        .getOrElse {
          val wt = worldParam.suggestions(source, extra, xs)
          if (wt._2.isEmpty) vector3dParam.suggestions(source, extra, xs) else wt
        }
    }
  }

  implicit def catalogedParam[A <: CatalogType](
      implicit classTag: ClassTag[A],
      typeable: Typeable[A]
  ): Parameter[Set[A]] =
    new Parameter[Set[A]] {
      private val clazz = classTag.runtimeClass.asInstanceOf[Class[A]]

      override def name: String = typeable.describe

      override def parse(
          source: CommandSource,
          extra: Unit,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], Set[A])] =
        ScammanderHelper.parseMany(
          name,
          xs,
          Sponge.getRegistry.getAllOf(clazz).asScala.map(obj => obj.getId.toLowerCase(Locale.ROOT) -> obj).toMap
        )

      override def suggestions(
          source: CommandSource,
          extra: Location[World],
          xs: List[RawCmdArg]
      ): (List[RawCmdArg], Seq[String]) =
        ScammanderHelper.suggestions(xs, Sponge.getRegistry.getAllOf(clazz).asScala.map(_.getId))
    }

  implicit val pluginParam: Parameter[Set[PluginContainer]] = new Parameter[Set[PluginContainer]] {
    override def name: String = "plguin"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[PluginContainer])] =
      ScammanderHelper.parseMany(
        name,
        xs,
        Sponge.getPluginManager.getPlugins.asScala.map(obj => obj.getId.toLowerCase(Locale.ROOT) -> obj).toMap
      )

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) =
      ScammanderHelper.suggestions(xs, Sponge.getPluginManager.getPlugins.asScala.map(_.getId))
  }

  implicit val ipParam: Parameter[InetAddress] = new Parameter[InetAddress] {
    override def name: String = "ip"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], InetAddress)] = {
      if (xs.nonEmpty) {
        val RawCmdArg(pos, _, arg) = xs.head
        Try {
          xs.tail -> InetAddress.getByName(arg)
        }.toEither.left.flatMap {
          case _: UnknownHostException =>
            playerParam.parse(source, extra, xs).map {
              case (ys, player) =>
                ys -> player.getConnection.getAddress.getAddress
            }
          case e => Left(CommandUsageError(e.getMessage, pos))
        }

      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = (xs.tail, Nil)
  }

  implicit val dataContainerParam: Parameter[DataContainer] = new Parameter[DataContainer] {
    override def name: String = "dataContainer"
    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], DataContainer)] = {
      remainingAsStringParam.parse(source, extra, xs).flatMap {
        case (ys, RemainingAsString(str)) =>
          val reader: Callable[BufferedReader] = () => new BufferedReader(new StringReader(str))
          val loader = HoconConfigurationLoader.builder.setSource(reader).build

          Try {
            ys -> DataTranslators.CONFIGURATION_NODE.translate(loader.load())
          }.toEither.left.map { e =>
            CommandSyntaxError(e.getMessage, xs.lastOption.map(_.start).getOrElse(-1))
          }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): (List[RawCmdArg], Seq[String]) = (xs.tail, Nil)
  }

  //TODO: text

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

    override def usage(source: CommandSource): String =
      targeter.getTarget(source, -1).map(_ => s"[$name]").getOrElse(super.usage(source))
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

  implicit val playerSender: UserValidator[Player] = UserValidator.mkValidator {
    case player: Player     => Right(player)
    case proxy: ProxySource => playerSender.validate(proxy.getOriginalSource)
    case _                  => Left(CommandUsageError("This command can only be used by players", -1))
  }

  implicit val userSender: UserValidator[User] = UserValidator.mkValidator {
    case user: User         => Right(user)
    case proxy: ProxySource => userSender.validate(proxy.getOriginalSource)
    case _                  => Left(CommandUsageError("This command can only be used by users", -1))
  }

  implicit def entitySender[A <: Entity: Typeable]: UserValidator[A] = {
    val EntityCase = TypeCase[A]

    UserValidator.mkValidator {
      case EntityCase(entity) => Right(entity)
      case proxy: ProxySource => entitySender.validate(proxy.getOriginalSource)
      case _                  => Left(CommandUsageError("This command can only be used by players", -1))
    }
  }

  implicit val locationSender: UserValidator[Location[World]] = UserValidator.mkValidator {
    case locatable: Locatable => Right(locatable.getLocation)
    case proxy: ProxySource   => locationSender.validate(proxy.getOriginalSource)
    case _                    => Left(CommandUsageError("This command can only be used by things which have a location", -1))
  }

  implicit val vector3dSender: UserValidator[Vector3d] =
    UserValidator.mkValidator(locationSender.validate(_).map(_.getPosition))

  implicit val ipSender: UserValidator[InetAddress] = UserValidator.mkValidator {
    case remote: RemoteSource => Right(remote.getConnection.getAddress.getAddress)
    case proxy: ProxySource   => ipSender.validate(proxy.getOriginalSource)
    case _                    => Left(CommandUsageError("This command can only be used by things which have an IP", -1))
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
      command.suggestions(source, targetPosition, ScammanderHelper.stringToRawArgs(arguments)).asJava

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
