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
import org.spongepowered.api.command._
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

import net.katsstuff.scammander.misc.{HasName, RawCmdArg}
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import ninja.leaping.configurate.hocon.HoconConfigurationLoader
import shapeless._

trait SpongeUniverse extends ScammanderUniverse[CommandSource, Unit, Location[World]] {

  override protected type Result             = Int
  override protected type StaticChildCommand = SpongeCommandWrapper[_, _]

  override protected val defaultCommandSuccess: Int = 1

  //Helpers used when registering command

  override protected def tabExtraToRunExtra(extra: Location[World]): Unit = ()

  /**
    * Helper for creating an alias when registering a command.
    */
  object Alias {
    def apply(first: String, aliases: String*): Seq[String] = first +: aliases
  }

  /**
    * Helper for creating a alias when registering a command.
    */
  object Permission {
    def apply(perm: String): Some[String] = Some(perm)
    val none:                None.type    = None
  }

  /**
    * Helper for creating a help when registering a command.
    */
  object Help {
    def apply(f: CommandSource => Text): CommandSource => Option[Text] = f andThen Some.apply
    def apply(text: Text):               CommandSource => Option[Text] = _ => Some(text)
    val none:                            CommandSource => None.type    = _ => None
  }

  /**
    * Helper for creating an description when registering a command.
    */
  object Description {
    def apply(f: CommandSource => Text): CommandSource => Option[Text] = f andThen Some.apply
    def apply(text: Text):               CommandSource => Option[Text] = _ => Some(text)
    val none:                            CommandSource => None.type    = _ => None
  }

  /**
    * A class to use for parameter that should require a specific permission.
    */
  case class NeedPermission[S <: String, A](value: A)

  implicit def needPermissionParam[S <: String, A](
      implicit param0: Parameter[A],
      w: Witness.Aux[S]
  ): Parameter[NeedPermission[S, A]] =
    new ProxyParameter[NeedPermission[S, A], A] {
      override def param: Parameter[A] = param0

      val perm: String = w.value

      override def parse(
          source: CommandSource,
          extra: Unit,
          xs: List[RawCmdArg]
      ): CommandStep[(List[RawCmdArg], NeedPermission[S, A])] =
        if (source.hasPermission(perm)) param.parse(source, extra, xs).map(t => t._1 -> NeedPermission(t._2))
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
      ): Either[List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else Left(xs.drop(1))
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

        if (head.startsWith("@")) {
          try {
            val players = Selector
              .parse(xs.head.content)
              .resolve(source)
              .asScala
              .collect {
                case player: Player => player
              }
              .toSet

            Right(xs.tail -> players)
          } catch {
            case e: IllegalArgumentException => Left(Command.error(e.getMessage))
          }
        } else {
          ScammanderHelper.parseMany(name, xs, Sponge.getServer.getOnlinePlayers.asScala)
        }
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] = {
      if (xs.nonEmpty) {
        val head = xs.head
        val choices =
          if (head.content.startsWith("@")) Selector.complete(head.content).asScala
          else Sponge.getServer.getOnlinePlayers.asScala.map(_.getName)

        ScammanderHelper.suggestions(parse(source, (), _), xs, choices)
      } else ScammanderHelper.suggestionsNamed(parse(source, (), _), xs, Sponge.getServer.getOnlinePlayers.asScala)
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
        try {
          val entities = Selector.parse(xs.head.content).resolve(source).asScala.collect {
            case EntityType(entity) => entity
          }
          Right((xs.tail, entities.toSet))
        } catch {
          case e: IllegalArgumentException => Left(Command.error(e.getMessage))
        }
      } else Left(ScammanderHelper.notEnoughArgs)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] =
      if (xs.nonEmpty) {
        ScammanderHelper.suggestions(parse(source, (), _), xs, Selector.complete(xs.head.content).asScala)
      } else Left(Nil)
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

      for {
        e1 <- players.map(t => t._1 -> t._2.map(player => player: User)).left
        e2 <- users.left
      } yield e1.merge(e2)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] =
      ScammanderHelper.suggestions(parse(source, (), _), xs, userStorage.getAll.asScala.collect {
        case profile if profile.getName.isPresent => profile.getName.get()
      })
  }

  implicit val worldParam: Parameter[Set[WorldProperties]] =
    Parameter.mkNamed("world", Sponge.getServer.getAllWorldProperties.asScala)

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
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(3))

    private def parseRelativeDouble(
        source: CommandSource,
        xs: List[RawCmdArg],
        relativeToOpt: Option[Double]
    ): CommandStep[(List[RawCmdArg], Double)] = {
      val RawCmdArg(start, end, arg) = xs.head

      if (arg.startsWith("~")) {
        relativeToOpt
          .toRight(Command.usageError("Relative position specified but source does not have a position", start))
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
            try {
              val entities = Selector.parse(arg).resolve(source).asScala.toSet
              Right(xs.tail -> entities.map(_.getLocation))
            } catch {
              case e: IllegalArgumentException => Left(Command.error(e.getMessage))
            }
        }
        .getOrElse {
          for {
            wt <- for {
              e1 <- oneWorldParam.parse(source, extra, xs).map(t => t._1 -> t._2.value).left
              e2 <- locationSender.validate(source).map(pos => xs        -> pos.getExtent.getProperties).left
            } yield e1.merge(e2)
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
    ): Either[List[RawCmdArg], Seq[String]] = {
      xs.headOption
        .collect {
          case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
            try {
              Selector.parse(arg)
              Left(xs.tail)
            } catch {
              case _: IllegalArgumentException => Right(Selector.complete(arg).asScala)
            }
        }
        .getOrElse {
          worldParam.suggestions(source, extra, xs).left.flatMap(_ => vector3dParam.suggestions(source, extra, xs))
        }
    }
  }

  implicit def catalogedParam[A <: CatalogType](
      implicit classTag: ClassTag[A],
      typeable: Typeable[A]
  ): Parameter[Set[A]] = {
    val clazz = classTag.runtimeClass.asInstanceOf[Class[A]]
    Parameter.mkNamed(typeable.describe, Sponge.getRegistry.getAllOf(clazz).asScala)
  }

  implicit val pluginParam: Parameter[Set[PluginContainer]] =
    Parameter.mkNamed("plugin", Sponge.getPluginManager.getPlugins.asScala)

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
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
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
    ): Either[List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
  }

  //TODO: text

  implicit val playerHasName:                        HasName[Player]          = (a: Player) => a.getName
  implicit val worldHasName:                         HasName[WorldProperties] = (a: WorldProperties) => a.getWorldName
  implicit def catalogTypeHasName[A <: CatalogType]: HasName[A]               = (a: A) => a.getId
  implicit val pluginHasName:                        HasName[PluginContainer] = (a: PluginContainer) => a.getId

  /**
    * A typeclass which returns what the sender is currently targeting.
    */
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

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], OrTarget[Base])] = {
      val ret = for {
        e1 <- parameter.parse(source, extra, xs).left
        e2 <- targeter.getTarget(source, xs.headOption.map(_.start).getOrElse(-1)).map(xs -> _).left
      } yield e1.merge(e2)

      ret.map(t => t._1 -> Or(t._2))
    }

    override def usage(source: CommandSource): String =
      targeter.getTarget(source, -1).map(_ => s"[$name]").getOrElse(super.usage(source))
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toSponge(info: CommandInfo):                      SpongeCommandWrapper[Sender, Param] = SpongeCommandWrapper(command, info)
    def toChild(aliases: Seq[String], info: CommandInfo): ChildCommand                        = ChildCommand(aliases.toSet, toSponge(info))
    def toChild(
        aliases: Seq[String],
        permission: Option[String] = None,
        help: CommandSource => Option[Text] = _ => None,
        shortDescription: CommandSource => Option[Text] = _ => None
    ): ChildCommand = ChildCommand(aliases.toSet, toSponge(CommandInfo(permission, help, shortDescription)))

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

    override def process(source: CommandSource, arguments: String): CommandResult = {
      val args = ScammanderHelper.stringToRawArgsQuoted(arguments)

      if (args.nonEmpty && command.childrenMap.contains(args.head.content)) {
        val childCommand = command.childrenMap(args.head.content)
        if (childCommand.testPermission(source)) {
          childCommand.process(source, args.tail.mkString(" "))
        } else {
          throw new CommandPermissionException
        }
      } else {
        val res = for {
          sender <- command.userValidator.validate(source)
          param  <- command.par.parse(source, (), args)
          result <- command.run(sender, (), param._2)
        } yield result

        res match {
          case Right(CommandSuccess(count)) => CommandResult.successCount(count)
          case Left(CommandError(msg))      => throw new CommandException(Text.of(msg))
          case Left(CommandSyntaxError(msg, pos)) =>
            val e =
              if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
              else new CommandException(Text.of(msg))
            throw e
          case Left(CommandUsageError(msg, pos)) =>
            //TODO: Custom exception
            val e =
              if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
              else new CommandException(Text.of(msg))
            throw e
          case Left(e: MultipleCommandErrors) => throw new CommandException(Text.of(e.msg)) //TODO: Better error here
        }
      }
    }

    override def getSuggestions(
        source: CommandSource,
        arguments: String,
        targetPosition: Location[World]
    ): util.List[String] = {
      val args = ScammanderHelper.stringToRawArgsQuoted(arguments)

      if (args.nonEmpty && command.childrenMap.contains(args.head.content)) {
        val childCommand = command.childrenMap(args.head.content)
        if (childCommand.testPermission(source)) {
          childCommand.getSuggestions(source, args.tail.map(_.content).mkString(" "), targetPosition)
        } else {
          Nil.asJava
        }
      } else {
        val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = xs => {
          val isParsed = xs.headOption.exists(arg => command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content)))
          Either.cond(isParsed, (xs.drop(1), true), Command.error("Not child"))
        }
        val childSuggestions = ScammanderHelper.suggestions(parse, args, command.childrenMap.keys)
        val ret =
          if (args.nonEmpty && childSuggestions.isRight) childSuggestions.getOrElse(Nil)
          else {
            val paramSuggestions = command.suggestions(source, targetPosition, args)
            childSuggestions.getOrElse(Nil) ++ paramSuggestions
          }

        ret.asJava
      }
    }

    override def testPermission(source: CommandSource): Boolean = info.permission.forall(source.hasPermission)

    override def getShortDescription(source: CommandSource): Optional[Text] = info.shortDescription(source) match {
      case Some(description) => Optional.of(description)
      case None              => Optional.empty()
    }

    override def getHelp(source: CommandSource): Optional[Text] = info.help(source) match {
      case Some(help) => Optional.of(help)
      case None       => Optional.empty()
    }

    override def getUsage(source: CommandSource): Text = {
      if (command.children.nonEmpty) {
        val childUsages = command.children.map {
          case ChildCommand(aliases, childCommand) =>
            val aliasPart = aliases.mkString("|")
            val usagePart = childCommand.getUsage(source)
            s"$aliasPart $usagePart"
        }

        val childUsage = childUsages.mkString("|")
        val usage      = s"$childUsage|${command.usage(source)}"
        Text.of(usage)
      } else {
        Text.of(command.usage(source))
      }
    }

    def register(plugin: AnyRef, aliases: Seq[String]): Option[CommandMapping] = {
      val res = Sponge.getCommandManager.register(plugin, this, aliases.asJava)
      if (res.isPresent) Some(res.get()) else None
    }
  }
}
