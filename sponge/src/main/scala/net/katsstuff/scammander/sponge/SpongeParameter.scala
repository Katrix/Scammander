package net.katsstuff.scammander.sponge

import java.io.{BufferedReader, StringReader}
import java.net.{InetAddress, UnknownHostException}
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

import net.katsstuff.scammander.CrossCompatibility._
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderBase, ScammanderHelper}
import ninja.leaping.configurate.hocon.HoconConfigurationLoader
import shapeless.{TypeCase, Typeable, Witness}

trait SpongeParameter {
  self: ScammanderBase[CommandSource, Unit, Location[World]]
    with NormalParameters[CommandSource, Unit, Location[World]]
    with HelperParameters[CommandSource, Unit, Location[World]]
    with SpongeValidators =>

  implicit val playerHasName:                        HasName[Player]          = HasName.instance((a: Player) => a.getName)
  implicit val worldHasName:                         HasName[WorldProperties] = HasName.instance((a: WorldProperties) => a.getWorldName)
  implicit def catalogTypeHasName[A <: CatalogType]: HasName[A]               = HasName.instance((a: A) => a.getId)
  implicit val pluginHasName:                        HasName[PluginContainer] = HasName.instance((a: PluginContainer) => a.getId)

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
      ): EitherT[F, List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else Left(xs.drop(1))
    }

  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {
    override def name: String = "player"

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[Player])] = {
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, head) =>
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

              Right(ys -> players)
            } catch {
              case e: IllegalArgumentException => Left(Command.error(e.getMessage))
            }
          } else {
            ScammanderHelper.parseMany(name, xs, Sponge.getServer.getOnlinePlayers.asScala)
          }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): EitherT[F, List[RawCmdArg], Seq[String]] = {
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
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, arg) =>
          try {
            val entities = Selector.parse(arg).resolve(source).asScala.collect {
              case EntityType(entity) => entity
            }
            Right((ys, entities.toSet))
          } catch {
            case e: IllegalArgumentException => Left(Command.error(e.getMessage))
          }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): EitherT[F, List[RawCmdArg], Seq[String]] =
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
    ): EitherT[F, List[RawCmdArg], Seq[String]] =
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
    ): EitherT[F, List[RawCmdArg], Seq[String]] = Left(xs.drop(3))

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
    ): EitherT[F, List[RawCmdArg], Seq[String]] = {
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
      stringParam.parse(source, extra, xs).flatMap {
        case (ys, arg) =>
          Try {
            ys -> InetAddress.getByName(arg)
          }.toEither.left.flatMap {
            case _: UnknownHostException =>
              playerParam.parse(source, extra, xs).map {
                case (zs, player) =>
                  zs -> player.getConnection.getAddress.getAddress
              }
            case e => Left(CommandUsageError(e.getMessage, xs.head.start))
          }
      }
    }

    override def suggestions(
        source: CommandSource,
        extra: Location[World],
        xs: List[RawCmdArg]
    ): EitherT[F, List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
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
          //noinspection ConvertExpressionToSAM
          val reader: Callable[BufferedReader] = new Callable[BufferedReader] {
            override def call(): BufferedReader = new BufferedReader(new StringReader(str))
          }
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
    ): EitherT[F, List[RawCmdArg], Seq[String]] = Left(xs.drop(1))
  }

  //TODO: text
}
