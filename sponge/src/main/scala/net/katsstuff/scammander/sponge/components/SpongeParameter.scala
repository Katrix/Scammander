package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds

import java.io.{BufferedReader, StringReader}
import java.net.InetAddress
import java.util.Locale
import java.util.concurrent.Callable

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import cats.Eval
import cats.syntax.all._
import com.flowpowered.math.vector.Vector3d
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderHelper}
import ninja.leaping.configurate.hocon.HoconConfigurationLoader
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
import shapeless.{TypeCase, Typeable, Witness}

trait SpongeParameter {
  self: SpongeBase with NormalParameters with HelperParameters with SpongeValidators =>

  implicit val playerHasName: HasName[Player]                   = HasName.instance((a: Player) => a.getName)
  implicit val worldHasName: HasName[WorldProperties]           = HasName.instance((a: WorldProperties) => a.getWorldName)
  implicit def catalogTypeHasName[A <: CatalogType]: HasName[A] = HasName.instance((a: A) => a.getId)
  implicit val pluginHasName: HasName[PluginContainer]          = HasName.instance((a: PluginContainer) => a.getId)

  private def tryToEither[B](tried: Try[B]): Either[Throwable, B] = tried match {
    case Success(b) => Right(b)
    case Failure(e) => Left(e)
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

      def permError[F[_]: ParserError, B](pos: Int): F[B] =
        Result.usageErrorF[F, B]("You do not have the permissions needed to use this parameter", pos)

      override def parse[F[_]: ParserState: ParserError](
          source: CommandSource,
          extra: Unit
      ): F[NeedPermission[S, A]] =
        if (source.hasPermission(perm)) param.parse(source, extra).map(NeedPermission.apply)
        else ScammanderHelper.getPos.flatMap(permError[F, NeedPermission[S, A]])

      override def suggestions[F[_]: ParserState: ParserError](
          source: CommandSource,
          extra: Option[Location[World]]
      ): F[Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra)
        else ScammanderHelper.dropFirstArg[F]
    }

  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {

    override val name: String = "player"

    private def parseSelector[F[_]: ParserState: ParserError](source: CommandSource, arg: RawCmdArg): F[Set[Player]] = {
      try {
        Selector
          .parse(arg.content)
          .resolve(source)
          .asScala
          .collect { case player: Player => player }
          .toSet
          .pure[F]
      } catch {
        case e: IllegalArgumentException => Result.errorF[F, Set[Player]](e.getMessage)
      }
    }

    override def parse[F[_]: ParserState: ParserError](source: CommandSource, extra: Unit): F[Set[Player]] =
      for {
        arg <- ScammanderHelper.firstArg
        res <- {
          if (arg.content.startsWith("@")) parseSelector(source, arg)
          else ScammanderHelper.parseMany[F, Player](name, Sponge.getServer.getOnlinePlayers.asScala)
        }
        _ <- ScammanderHelper.dropFirstArg[F]
      } yield res

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] = ScammanderHelper.firstArgOpt.flatMap {
      case Some(arg) =>
        lazy val choices =
          if (arg.content.startsWith("@")) Selector.complete(arg.content).asScala
          else Sponge.getServer.getOnlinePlayers.asScala.map(_.getName)

        ScammanderHelper.suggestions(parse(source, ()), Eval.later(choices))
      case None =>
        ScammanderHelper.suggestionsNamed(parse(source, ()), Eval.later(Sponge.getServer.getOnlinePlayers.asScala))
    }
  }

  implicit val playerParam: Parameter[Player] = Parameter[OnlyOne[Player]].map(_.value)

  implicit def entityParam[A <: Entity](implicit typeable: Typeable[A]): Parameter[Set[A]] = new Parameter[Set[A]] {

    override val name: String = "entity"

    private val EntityType: TypeCase[A] = TypeCase[A]

    override def parse[F[_]: ParserState: ParserError](source: CommandSource, extra: Unit): F[Set[A]] =
      ScammanderHelper.firstArg.flatMap { arg =>
        try {
          val entities = Selector.parse(arg.content).resolve(source).asScala.collect {
            case EntityType(entity) => entity
          }
          entities.toSet.pure[F]
        } catch {
          case e: IllegalArgumentException => Result.errorF[F, Set[A]](e.getMessage)
        }
      }

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] =
      ScammanderHelper.firstArgOpt.flatMap {
        case Some(arg) =>
          ScammanderHelper.suggestions(parse(source, ()), Eval.later(Selector.complete(arg.content).asScala))
        case None => ScammanderHelper.dropFirstArg[F]
      }
  }

  implicit val userParam: Parameter[Set[User]] = new Parameter[Set[User]] {
    private val userStorage = Sponge.getServiceManager.provideUnchecked(classOf[UserStorageService])

    override val name: String = "user"

    private def parsePlayers[F[_]: ParserState: ParserError](source: CommandSource): F[Set[User]] =
      allPlayerParam.parse(source, ()).map(_.map(player => player: User))

    private def parseUsers[F[_]: ParserState: ParserError]: F[Set[User]] = {
      val users = userStorage.getAll.asScala
        .collect {
          case profile if profile.getName.isPresent =>
            val name = profile.getName.get().toLowerCase(Locale.ROOT)
            name -> name
        }
        .toMap
        .filterKeys(userStorage.get(_).isPresent) //filter and map here are lazy, so we only do as many lookups as needed
        .mapValues(userStorage.get(_).get())

      ScammanderHelper.parseMany[F, User](name, users)
    }

    override def parse[F[_]: ParserState: ParserError](source: CommandSource, extra: Unit): F[Set[User]] =
      ScammanderHelper.withFallback(parsePlayers(source), parseUsers)

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] =
      ScammanderHelper.suggestions(parse(source, ()), Eval.later {
        userStorage.getAll.asScala.collect {
          case profile if profile.getName.isPresent => profile.getName.get()
        }
      })
  }

  implicit val worldParam: Parameter[Set[WorldProperties]] =
    Parameter.mkNamed("world", Sponge.getServer.getAllWorldProperties.asScala)

  implicit val vector3dParam: Parameter[Vector3d] = new Parameter[Vector3d] {
    override def name: String = "vector3"
    override def parse[F[_]: ParserState: ParserError](source: CommandSource, extra: Unit): F[Vector3d] = {
      val relative = source match {
        case locatable: Locatable => Some(locatable.getLocation)
        case _                    => None
      }

      for {
        x <- parseRelativeOrNormal(source, relative.map(_.getX))
        t <- parseRelativeOrNormal(source, relative.map(_.getY))
        z <- parseRelativeOrNormal(source, relative.map(_.getZ))
      } yield Vector3d.from(x, t, z)
    }

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] =
      ScammanderHelper.dropFirstArg[F] *> ScammanderHelper.dropFirstArg[F] *> ScammanderHelper.dropFirstArg[F]

    private def hasNoPosError(pos: Int): CommandFailureNEL =
      Result.usageErrorNel("Relative position specified but source does not have a position", pos)

    private def parseRelative[F[_]](
        source: CommandSource,
        relativeToOpt: Option[Double],
        arg: RawCmdArg
    )(implicit S: ParserState[F], E: ParserError[F]): F[Double] =
      E.fromEither(relativeToOpt.toRight(hasNoPosError(arg.start)))
        .flatMap { relativeTo =>
          val newArg = arg.content.substring(1)
          if (newArg.isEmpty) relativeTo.pure[F]
          else
            S.modify(xs => xs.headOption.fold(xs)(head => head.copy(content = newArg) :: xs.tail)) *>
              doubleParam
                .parse(source, ())
                .map(_ + relativeTo)
        }

    private def parseRelativeOrNormal[F[_]: ParserState: ParserError](
        source: CommandSource,
        relativeToOpt: Option[Double]
    ): F[Double] =
      for {
        arg <- ScammanderHelper.firstArg
        res <- {
          if (arg.content.startsWith("~")) parseRelative(source, relativeToOpt, arg)
          else doubleParam.parse(source, ())
        }
      } yield res
  }

  implicit val locationParam: Parameter[Set[Location[World]]] = new Parameter[Set[Location[World]]] {
    private val oneWorldParam = Parameter[OnlyOne[WorldProperties]]

    override val name: String = "location"

    override def parse[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Unit
    ): F[Set[Location[World]]] = ScammanderHelper.firstArgOpt.flatMap { cmdArg =>
      cmdArg
        .collect {
          case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
            try {
              val entities = Selector.parse(arg).resolve(source).asScala.toSet
              entities.map(_.getLocation).pure[F]
            } catch {
              case e: IllegalArgumentException =>
                Result.errorF[F, Set[Location[World]]](e.getMessage)
            }
        }
        .getOrElse {
          val worldfromParam    = oneWorldParam.parse(source, extra)
          lazy val worldFromLoc = locationSender.validate[F](source).map(pos => OnlyOne(pos.getExtent.getProperties))

          val parseWorld = ScammanderHelper.withFallback(worldfromParam, worldFromLoc)

          for {
            worldProp <- parseWorld
            loc       <- vector3dParam.parse(source, extra)
          } yield {
            val world = Sponge.getServer.getWorld(worldProp.value.getUniqueId).get()
            Set(new Location[World](world, loc))
          }
        }
    }

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] = ScammanderHelper.firstArgOpt.flatMap { cmdArg =>
      cmdArg
        .collect {
          case RawCmdArg(_, _, arg) if arg.startsWith("@") =>
            try {
              Selector.parse(arg)
              ScammanderHelper.dropFirstArg[F]
            } catch {
              case _: IllegalArgumentException =>
                (Selector.complete(arg).asScala: Seq[String]).pure[F]
            }
        }
        .getOrElse {
          ScammanderHelper.withFallbackSuggestions(
            worldParam.suggestions(source, extra),
            vector3dParam.suggestions(source, extra)
          )
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

    override val name: String = "ip"

    override def parse[F[_]](
        source: CommandSource,
        extra: Unit
    )(implicit S: ParserState[F], E: ParserError[F]): F[InetAddress] = {
      val fromIp = ScammanderHelper.firstArgAndDrop.flatMap { arg =>
        E.fromEither(tryToEither(Try {
          InetAddress.getByName(arg.content)
        }).left.map(e => Result.usageErrorNel(e.getMessage, arg.start)))
      }
      val fromPlayer = playerParam.parse(source, extra).map(_.getConnection.getAddress.getAddress)

      ScammanderHelper.withFallback(fromIp, fromPlayer)
    }

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] = ScammanderHelper.dropFirstArg[F]
  }

  implicit val dataContainerParam: Parameter[DataContainer] = new Parameter[DataContainer] {
    override def name: String = "dataContainer"
    override def parse[F[_]](
        source: CommandSource,
        extra: Unit
    )(implicit S: ParserState[F], E: ParserError[F]): F[DataContainer] = {
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

          E.fromEither(
            tryToEither(Try(DataTranslators.CONFIGURATION_NODE.translate(loader.load()))).left.map { e =>
              Result.syntaxErrorNel(e.getMessage, pos)
            }
          )
        }
      } yield res
    }

    override def suggestions[F[_]: ParserState: ParserError](
        source: CommandSource,
        extra: Option[Location[World]]
    ): F[Seq[String]] = ScammanderHelper.dropFirstArg[F]
  }

  //TODO: text
}
