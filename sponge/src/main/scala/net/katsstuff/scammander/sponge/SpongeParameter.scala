package net.katsstuff.scammander.sponge

import java.io.{BufferedReader, StringReader}
import java.net.InetAddress
import java.util.Locale
import java.util.concurrent.Callable

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.util.Try

import org.spongepowered.api.command.CommandSource
import org.spongepowered.api.data.DataContainer
import org.spongepowered.api.data.persistence.DataTranslators
import org.spongepowered.api.entity.Entity
import org.spongepowered.api.entity.living.player.{Player, User}
import org.spongepowered.api.plugin.PluginContainer
import org.spongepowered.api.service.user.UserStorageService
import org.spongepowered.api.text.selector.Selector
import org.spongepowered.api.world.storage.WorldProperties
import org.spongepowered.api.world.{Locatable, Location, World}
import org.spongepowered.api.{CatalogType, Sponge}

import com.flowpowered.math.vector.Vector3d

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import net.katsstuff.scammander
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderBase, ScammanderHelper}
import ninja.leaping.configurate.hocon.HoconConfigurationLoader
import shapeless.{TypeCase, Typeable, Witness}

trait SpongeParameter {
  self: ScammanderBase[
    ({ type L[A] = Either[NonEmptyList[scammander.CommandFailure], A] })#L,
    CommandSource,
    Unit,
    Location[World]
  ] with NormalParameters[
      ({ type L[A] = Either[NonEmptyList[scammander.CommandFailure], A] })#L,
      CommandSource,
      Unit,
      Location[World]
    ]
    with HelperParameters[
      ({ type L[A] = Either[NonEmptyList[scammander.CommandFailure], A] })#L,
      CommandSource,
      Unit,
      Location[World]
    ]
    with SpongeValidators =>

  implicit val playerHasName: HasName[Player]                   = HasName.instance((a: Player) => a.getName)
  implicit val worldHasName: HasName[WorldProperties]           = HasName.instance((a: WorldProperties) => a.getWorldName)
  implicit def catalogTypeHasName[A <: CatalogType]: HasName[A] = HasName.instance((a: A) => a.getId)
  implicit val pluginHasName: HasName[PluginContainer]          = HasName.instance((a: PluginContainer) => a.getId)

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
          extra: Unit
      ): StateT[CommandStep, List[RawCmdArg], NeedPermission[S, A]] =
        if (source.hasPermission(perm)) param.parse(source, extra).map(NeedPermission.apply)
        else
          ScammanderHelper.getPos.flatMapF { pos =>
            Command.usageErrorF("You do not have the permissions needed to use this parameter", pos)
          }

      override def suggestions(
          source: CommandSource,
          extra: Location[World]
      ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra)
        else ScammanderHelper.dropFirstArg
    }

  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {
    override def name: String = "player"

    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], Set[Player]] =
      for {
        arg <- ScammanderHelper.firstArg
        res <- {
          if (arg.content.startsWith("@")) {
            try {
              val players = Selector
                .parse(arg.content)
                .resolve(source)
                .asScala
                .collect {
                  case player: Player => player
                }
                .toSet

              SF.pure(players)
            } catch {
              case e: IllegalArgumentException =>
                Command.errorState(e.getMessage)
            }
          } else {
            ScammanderHelper.parseMany(name, Sponge.getServer.getOnlinePlayers.asScala)
          }
        }
        _ <- ScammanderHelper.dropFirstArg
      } yield res

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] = {
      ScammanderHelper.firstArgOpt.flatMap {
        case Some(arg) =>
          val choices =
            if (arg.content.startsWith("@")) Selector.complete(arg.content).asScala
            else Sponge.getServer.getOnlinePlayers.asScala.map(_.getName)

          ScammanderHelper.suggestions(parse(source, ()), choices)
        case None => ScammanderHelper.suggestionsNamed(parse(source, ()), Sponge.getServer.getOnlinePlayers.asScala)
      }
    }
  }

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], Player] =
      param.parse(source, extra).map(_.value)
  }

  implicit def entityParam[A <: Entity](implicit typeable: Typeable[A]): Parameter[Set[A]] = new Parameter[Set[A]] {
    override def name: String = "entity"

    private val EntityType: TypeCase[A] = TypeCase[A]

    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], Set[A]] =
      ScammanderHelper.firstArg.flatMapF { arg =>
        try {
          val entities = Selector.parse(arg.content).resolve(source).asScala.collect {
            case EntityType(entity) => entity
          }
          entities.toSet.pure
        } catch {
          case e: IllegalArgumentException => Command.errorF(e.getMessage)
        }
      }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
      ScammanderHelper.firstArgOpt.flatMap {
        case Some(arg) => ScammanderHelper.suggestions(parse(source, ()), Selector.complete(arg.content).asScala)
        case None      => ScammanderHelper.dropFirstArg
      }
  }

  implicit val userParam: Parameter[Set[User]] = new Parameter[Set[User]] {
    private val userStorage   = Sponge.getServiceManager.provideUnchecked(classOf[UserStorageService])
    override def name: String = "user"
    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], Set[User]] = {
      val players = allPlayerParam.parse(source, extra).map(_.map(player => player: User))
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

        ScammanderHelper.parseMany(name, users)
      }

      ScammanderHelper.withFallbackState(players, users)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
      ScammanderHelper.suggestions(parse(source, ()), userStorage.getAll.asScala.collect {
        case profile if profile.getName.isPresent => profile.getName.get()
      })
  }

  implicit val worldParam: Parameter[Set[WorldProperties]] =
    Parameter.mkNamed("world", Sponge.getServer.getAllWorldProperties.asScala)

  implicit val vector3dParam: Parameter[Vector3d] = new Parameter[Vector3d] {
    override def name: String = "vector3"
    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], Vector3d] = {
      val relative = source match {
        case locatable: Locatable => Some(locatable.getLocation)
        case _                    => None
      }

      for {
        x <- parseRelativeDouble(source, relative.map(_.getX))
        t <- parseRelativeDouble(source, relative.map(_.getY))
        z <- parseRelativeDouble(source, relative.map(_.getZ))
      } yield Vector3d.from(x, t, z)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
      ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg

    private def parseRelativeDouble(
        source: CommandSource,
        relativeToOpt: Option[Double]
    ): StateT[CommandStep, List[RawCmdArg], Double] =
      for {
        arg <- ScammanderHelper.firstArg[CommandStep]
        res <- {
          if (arg.content.startsWith("~")) {
            Command
              .liftEitherStateParse(
                relativeToOpt.toRight(
                  Command.usageErrorNel("Relative position specified but source does not have a position", arg.start)
                )
              )
              .flatMap { relativeTo =>
                val newArg = arg.content.substring(1)
                if (newArg.isEmpty) SF.pure(relativeTo)
                else {
                  doubleParam
                    .parse(source, ())
                    .contramap[List[RawCmdArg]](
                      xs => xs.headOption.fold(xs)(head => head.copy(content = newArg) :: xs.tail)
                    )
                    .map(_ + relativeTo)
                }
              }

          } else doubleParam.parse(source, ())
        }
      } yield res
  }

  implicit val locationParam: Parameter[Set[Location[World]]] = new Parameter[Set[Location[World]]] {
    private val oneWorldParam = Parameter[OnlyOne[WorldProperties]]
    override def name: String = "location"

    override def parse(
        source: CommandSource,
        extra: Unit
    ): StateT[CommandStep, List[RawCmdArg], Set[Location[World]]] = {
      ScammanderHelper.firstArgOpt.flatMap { cmdArg =>
        cmdArg
          .collect {
            case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
              try {
                val entities = Selector.parse(arg).resolve(source).asScala.toSet
                SF.pure(entities.map(_.getLocation))
              } catch {
                case e: IllegalArgumentException =>
                  Command.errorState[Set[Location[World]]](e.getMessage)
              }
          }
          .getOrElse {
            val worldfromParam = oneWorldParam.parse(source, extra)
            lazy val worldFromLoc =
              Command.liftFStateParse(locationSender.validate(source).map(pos => OnlyOne(pos.getExtent.getProperties)))

            val parseWorld = ScammanderHelper.withFallbackState(worldfromParam, worldFromLoc)

            for {
              worldProp <- parseWorld
              loc       <- vector3dParam.parse(source, extra)
            } yield {
              val world = Sponge.getServer.getWorld(worldProp.value.getUniqueId).get()
              Set(new Location[World](world, loc))
            }
          }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] = {
      ScammanderHelper.firstArgOpt.flatMap { cmdArg =>
        cmdArg
          .collect {
            case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
              try {
                Selector.parse(arg)
                ScammanderHelper.dropFirstArg[CommandStep]
              } catch {
                case _: IllegalArgumentException =>
                  SF.pure[Seq[String]](Selector.complete(arg).asScala)
              }
          }
          .getOrElse {
            ScammanderHelper.fallbackSuggestions(
              worldParam.suggestions(source, extra),
              vector3dParam.suggestions(source, extra)
            )
          }
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

    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], InetAddress] = {
      val fromIp = ScammanderHelper.firstArgAndDrop.flatMapF { arg =>
        Try {
          InetAddress.getByName(arg.content)
        }.toEither.left.map(e => Command.usageErrorNel(e.getMessage, arg.start))
      }
      val fromPlayer = playerParam.parse(source, extra).map(_.getConnection.getAddress.getAddress)

      ScammanderHelper.withFallbackState(fromIp, fromPlayer)
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] = ScammanderHelper.dropFirstArg
  }

  implicit val dataContainerParam: Parameter[DataContainer] = new Parameter[DataContainer] {
    override def name: String = "dataContainer"
    override def parse(source: CommandSource, extra: Unit): StateT[CommandStep, List[RawCmdArg], DataContainer] = {
      for {
        pos       <- ScammanderHelper.getPos
        remaining <- remainingAsStringParam.parse(source, extra)
        res <- {
          val RemainingAsString(str) = remaining
          //noinspection ConvertExpressionToSAM
          val reader: Callable[BufferedReader] = new Callable[BufferedReader] {
            override def call(): BufferedReader = new BufferedReader(new StringReader(str))
          }
          val loader = HoconConfigurationLoader.builder.setSource(reader).build

          Command.liftEitherStateParse(
            Try(DataTranslators.CONFIGURATION_NODE.translate(loader.load())).toEither.left.map { e =>
              Command.syntaxErrorNel(e.getMessage, pos)
            }
          )
        }
      } yield res
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World]
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] = ScammanderHelper.dropFirstArg
  }

  //TODO: text
}
