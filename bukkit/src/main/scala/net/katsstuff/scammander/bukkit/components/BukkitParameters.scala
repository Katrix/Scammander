package net.katsstuff.scammander.bukkit.components

import scala.language.higherKinds

import scala.collection.JavaConverters._

import cats.syntax.all._
import cats.{Eval, Monad}
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderHelper}
import org.bukkit.command.{BlockCommandSender, CommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.plugin.Plugin
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Bukkit, OfflinePlayer, World}
import shapeless.Witness

trait BukkitParameters {
  self: BukkitBase with NormalParameters with HelperParameters =>

  implicit val playerHasName: HasName[Player]               = HasName.instance((a: Player) => a.getName)
  implicit val offlinePlayerHasName: HasName[OfflinePlayer] = HasName.instance((a: OfflinePlayer) => a.getName)
  implicit val worldHasName: HasName[World]                 = HasName.instance((a: World) => a.getName)
  implicit val pluginHasName: HasName[Plugin]               = HasName.instance((a: Plugin) => a.getName)

  /**
    * A class to use for parameter that should require a specific permission.
    */
  case class NeedPermission[Perm <: String, A](value: A)

  implicit def needPermissionParam[Perm <: String, A](
      implicit param0: Parameter[A],
      w: Witness.Aux[Perm]
  ): Parameter[NeedPermission[Perm, A]] = new ProxyParameter[NeedPermission[Perm, A], A] {
    override def param: Parameter[A] = param0

    val perm: String = w.value

    def permError[F[_]: ParserError, B](pos: Int): F[B] =
      Result.usageErrorF[F, B]("You do not have the permissions needed to use this parameter", pos)

    override def parse[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[NeedPermission[Perm, A]] =
      if (source.hasPermission(perm)) param.parse(source, extra).map(NeedPermission.apply)
      else ScammanderHelper.getPos.flatMap(permError[F, NeedPermission[Perm, A]])

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Seq[String]] =
      if (source.hasPermission(perm)) super.suggestions(source, extra)
      else ScammanderHelper.dropFirstArg
  }

  //TODO: Selector with NMS
  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {

    override val name: String = "player"

    override def parse[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Set[Player]] = {
      val normalNames = ScammanderHelper.parseMany[F, Player](name, Bukkit.getOnlinePlayers.asScala)
      val uuidPlayers = uuidParam
        .parse(source, extra)
        .flatMap(
          uuid =>
            Option(Bukkit.getPlayer(uuid))
              .fold[F[Player]](Result.errorF(s"No player with the UUID ${uuid.toString}"))(_.pure[F])
        )
        .map(player => Set(player))

      ScammanderHelper.withFallback(normalNames, uuidPlayers)
    }

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Seq[String]] =
      ScammanderHelper.suggestionsNamed[F, Player, Set[Player]](
        parse(source, tabExtraToRunExtra(extra)),
        Eval.now(Bukkit.getOnlinePlayers.asScala)
      )
  }

  implicit val playerParam: Parameter[Player] = Parameter[OnlyOne[Player]].map(_.value)

  //TODO: Entity selector with NMS

  implicit val offlinePlayerParam: Parameter[Set[OfflinePlayer]] = new Parameter[Set[OfflinePlayer]] {
    override def name: String = "offlinePlayer"

    override def parse[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Set[OfflinePlayer]] = {
      val players: F[Set[OfflinePlayer]] = allPlayerParam.parse(source, extra).map(_.map(p => p: OfflinePlayer))
      val users: F[Set[OfflinePlayer]]   = ScammanderHelper.parseMany(name, Bukkit.getOfflinePlayers)
      val uuidUsers = uuidParam
        .parse(source, extra)
        .flatMap { uuid =>
          Bukkit.getOfflinePlayers
            .find(_.getUniqueId == uuid)
            .fold[F[OfflinePlayer]](Result.errorF(s"No user with the UUID ${uuid.toString}"))(_.pure[F])
        }
        .map(user => Set(user))

      ScammanderHelper.withFallback(ScammanderHelper.withFallback(players, users), uuidUsers)
    }

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Seq[String]] = {
      val parse = ScammanderHelper.firstArgAndDrop.flatMap[Boolean] { arg =>
        val res = Bukkit.getOfflinePlayers.exists(obj => HasName(obj).equalsIgnoreCase(arg.content))
        if (res) true.pure[F] else Result.errorF("Not parsed")
      }

      ScammanderHelper.suggestionsNamed(parse, Eval.later(Bukkit.getOfflinePlayers.toSeq))
    }
  }

  implicit val worldParam: Parameter[Set[World]] = Parameter.mkNamed("world", Bukkit.getWorlds.asScala)

  implicit val vector3dParam: Parameter[BukkitVector] = new Parameter[BukkitVector] {
    override def name: String = "vector3d"

    override def parse[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[BukkitVector] = {
      val relative = source match {
        case entity: Entity                  => Some(entity.getLocation)
        case blockSender: BlockCommandSender => Some(blockSender.getBlock.getLocation)
        case _                               => None
      }

      for {
        x <- parseRelativeOrNormal(source, extra, relative.map(_.getX))
        y <- parseRelativeOrNormal(source, extra, relative.map(_.getY))
        z <- parseRelativeOrNormal(source, extra, relative.map(_.getZ))
      } yield new BukkitVector(x, y, z)
    }

    override def suggestions[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra
    ): F[Seq[String]] =
      ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg

    private def hasNoPosError(pos: Int): CommandFailureNEL =
      Result.usageErrorNel("Relative position specified but source does not have a position", pos)

    private def parseRelative[F[_]: Monad](
        source: CommandSender,
        extra: BukkitExtra,
        relativeToOpt: Option[Double],
        arg: RawCmdArg
    )(implicit S: ParserState[F], E: ParserError[F]): F[Double] =
      relativeToOpt
        .fold(E.raise[Double](hasNoPosError(arg.start)))(_.pure)
        .flatMap { relativeTo =>
          val newArg = arg.content.substring(1)
          if (newArg.isEmpty) relativeTo.pure[F]
          else
            doubleParam
              .parse(source, extra)
              .map(_ + relativeTo) <*
              S.modify(xs => xs.headOption.fold(xs)(_.copy(content = newArg) :: xs.tail))
        }

    private def parseRelativeOrNormal[F[_]: Monad: ParserState: ParserError](
        source: CommandSender,
        extra: BukkitExtra,
        relativeToOpt: Option[Double]
    ): F[Double] =
      for {
        arg <- ScammanderHelper.firstArg[F]
        res <- {
          if (arg.content.startsWith("~")) parseRelative(source, extra, relativeToOpt, arg)
          else doubleParam.parse(source, extra)
        }
      } yield res
  }

  implicit val pluginParam: Parameter[Set[Plugin]] = Parameter.mkNamed("plugin", Bukkit.getPluginManager.getPlugins)

}
