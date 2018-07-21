package net.katsstuff.scammander.bukkit.components

import scala.collection.JavaConverters._
import scala.language.higherKinds

import org.bukkit.command.{BlockCommandSender, CommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.plugin.Plugin
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Bukkit, OfflinePlayer, World}

import cats.Eval
import cats.syntax.all._
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderHelper}
import shapeless.Witness

trait BukkitParameters[F[_]] {
  self: BukkitBase[F] with NormalParameters[F] with HelperParameters[F] =>

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

    def permError[B](pos: Int): F[B] =
      Command.usageErrorF[B]("You do not have the permissions needed to use this parameter", pos)

    override def parse(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[NeedPermission[Perm, A]] =
      if (source.hasPermission(perm)) param.parse(source, extra).map(NeedPermission.apply)
      else ScammanderHelper.getPos[F].flatMapF(permError)

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[Seq[String]] =
      if (source.hasPermission(perm)) super.suggestions(source, extra)
      else ScammanderHelper.dropFirstArg[F]
  }

  //TODO: Selector with NMS
  implicit val allPlayerParam: Parameter[Set[Player]] = new Parameter[Set[Player]] {

    override val name: String = "player"

    override def parse(source: CommandSender, extra: BukkitExtra): Parser[Set[Player]] = {
      val normalNames = ScammanderHelper.parseMany[F, Player](name, Bukkit.getOnlinePlayers.asScala)
      val uuidPlayers = uuidParam
        .parse(source, extra)
        .flatMapF(uuid => Option(Bukkit.getPlayer(uuid)).toF(s"No player with the UUID ${uuid.toString}"))
        .map(player => Set(player))

      ScammanderHelper.withFallbackParser(normalNames, uuidPlayers)
    }

    override def suggestions(source: CommandSender, extra: BukkitExtra): Parser[Seq[String]] =
      ScammanderHelper.suggestionsNamed[F, Player, Set[Player]](
        parse(source, tabExtraToRunExtra(extra)),
        Eval.now(Bukkit.getOnlinePlayers.asScala)
      )
  }

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(source: CommandSender, extra: BukkitExtra): Parser[Player] =
      param.parse(source, extra).map(_.value)
  }

  //TODO: Entity selector with NMS

  implicit val offlinePlayerParam: Parameter[Set[OfflinePlayer]] = new Parameter[Set[OfflinePlayer]] {
    override def name: String = "offlinePlayer"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[Set[OfflinePlayer]] = {
      val players: Parser[Set[OfflinePlayer]] = allPlayerParam.parse(source, extra).map(_.map(p => p: OfflinePlayer))
      val users: Parser[Set[OfflinePlayer]]   = ScammanderHelper.parseMany(name, Bukkit.getOfflinePlayers)
      val uuidUsers = uuidParam
        .parse(source, extra)
        .flatMapF { uuid =>
          Bukkit.getOfflinePlayers.find(_.getUniqueId == uuid).toF(s"No user with the UUID ${uuid.toString}")
        }
        .map(user => Set(user))

      ScammanderHelper.withFallbackParser(ScammanderHelper.withFallbackParser(players, users), uuidUsers)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[Seq[String]] = {
      val parse = ScammanderHelper.firstArgAndDrop.flatMapF[Boolean] { arg =>
        val res = Bukkit.getOfflinePlayers.exists(obj => HasName(obj).equalsIgnoreCase(arg.content))
        if (res) F.pure(true) else Command.errorF("Not parsed")
      }

      ScammanderHelper.suggestionsNamed(parse, Eval.later(Bukkit.getOfflinePlayers.toSeq))
    }
  }

  implicit val worldParam: Parameter[Set[World]] = Parameter.mkNamed("world", Bukkit.getWorlds.asScala)

  implicit val vector3dParam: Parameter[BukkitVector] = new Parameter[BukkitVector] {
    override def name: String = "vector3d"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[BukkitVector] = {
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

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra
    ): Parser[Seq[String]] =
      ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg

    private def hasNoPosError(pos: Int): CommandFailureNEL =
      Command.usageErrorNel("Relative position specified but source does not have a position", pos)

    private def parseRelative(
        source: CommandSender,
        extra: BukkitExtra,
        relativeToOpt: Option[Double],
        arg: RawCmdArg
    ): Parser[Double] =
      Command
        .liftEitherToParser(relativeToOpt.toRight(hasNoPosError(arg.start)))
        .flatMap { relativeTo =>
          val newArg = arg.content.substring(1)
          if (newArg.isEmpty) parser.pure(relativeTo)
          else
            doubleParam
              .parse(source, extra)
              .contramap[List[RawCmdArg]](xs => xs.headOption.fold(xs)(_.copy(content = newArg) :: xs.tail))
              .map(_ + relativeTo)
        }

    private def parseRelativeOrNormal(
        source: CommandSender,
        extra: BukkitExtra,
        relativeToOpt: Option[Double]
    ): Parser[Double] =
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
