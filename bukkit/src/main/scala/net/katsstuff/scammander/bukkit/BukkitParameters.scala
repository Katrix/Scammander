package net.katsstuff.scammander.bukkit

import org.bukkit.{Bukkit, OfflinePlayer, World}
import org.bukkit.command.{BlockCommandSender, CommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.plugin.Plugin
import org.bukkit.util.{Vector => BukkitVector}

import scala.collection.JavaConverters._

import net.katsstuff.scammander.CrossCompatibility._
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderBase, ScammanderHelper}
import shapeless.Witness

trait BukkitParameters {
  self: ScammanderBase[CommandSender, BukkitExtra, BukkitExtra]
    with NormalParameters[CommandSender, BukkitExtra, BukkitExtra]
    with HelperParameters[CommandSender, BukkitExtra, BukkitExtra] =>

  implicit val playerHasName:        HasName[Player]        = HasName.instance((a: Player) => a.getName)
  implicit val offlinePlayerHasName: HasName[OfflinePlayer] = HasName.instance((a: OfflinePlayer) => a.getName)
  implicit val worldHasName:         HasName[World]         = HasName.instance((a: World) => a.getName)
  implicit val pluginHasName:        HasName[Plugin]        = HasName.instance((a: Plugin) => a.getName)

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
          source: CommandSender,
          extra: BukkitExtra,
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
          source: CommandSender,
          extra: BukkitExtra,
          xs: List[RawCmdArg]
      ): EitherT[F, List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra, xs) else Left(xs.drop(1))
    }

  //TODO: Selector with NMS
  implicit val allPlayerParam: Parameter[Set[Player]] = Parameter.mkNamed("player", Bukkit.getOnlinePlayers.asScala)

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Player)] = param.parse(source, extra, xs).map(t => t._1 -> t._2.value)
  }

  //TODO: Entity selector with NMS

  implicit val offlinePlayerParam: Parameter[Set[OfflinePlayer]] = new Parameter[Set[OfflinePlayer]] {
    override def name: String = "offlinePlayer"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], Set[OfflinePlayer])] = {
      val players = allPlayerParam.parse(source, extra, xs)
      val users   = ScammanderHelper.parseMany(name, xs, Bukkit.getOfflinePlayers)

      for {
        e1 <- players.map(t => t._1 -> t._2.map(player => player: OfflinePlayer)).left
        e2 <- users.left
      } yield e1.merge(e2)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): EitherT[F, List[RawCmdArg], Seq[String]] = {
      val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = args => {
        val res = args.headOption.exists { head =>
          Bukkit.getOfflinePlayers.exists(obj => HasName(obj).equalsIgnoreCase(head.content))
        }
        if (res) Right((args.tail, true)) else Left(Command.error("Not parsed"))
      }

      ScammanderHelper.suggestionsNamed(parse, xs, Bukkit.getOfflinePlayers)
    }
  }

  implicit val worldParam: Parameter[Set[World]] = Parameter.mkNamed("world", Bukkit.getWorlds.asScala)

  implicit val vector3dParam: Parameter[BukkitVector] = new Parameter[BukkitVector] {
    override def name: String = "vector3d"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], BukkitVector)] = {
      val relative = source match {
        case entity: Entity                  => Some(entity.getLocation)
        case blockSender: BlockCommandSender => Some(blockSender.getBlock.getLocation)
        case _                               => None
      }

      for {
        tx <- parseRelativeDouble(source, extra, xs, relative.map(_.getX))
        ty <- parseRelativeDouble(source, extra, tx._1, relative.map(_.getY))
        tz <- parseRelativeDouble(source, extra, ty._1, relative.map(_.getZ))
      } yield tz._1 -> new BukkitVector(tx._2, ty._2, tz._2)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra,
        xs: List[RawCmdArg]
    ): EitherT[F, List[RawCmdArg], Seq[String]] = Left(xs.drop(3))

    private def parseRelativeDouble(
        source: CommandSender,
        extra: BukkitExtra,
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
              doubleParam.parse(source, extra, RawCmdArg(start, end, newArg) :: xs.tail).map {
                case (ys, res) =>
                  ys -> (res + relativeTo)
              }
            }
          }
      } else {
        doubleParam.parse(source, extra, xs)
      }
    }
  }

  implicit val pluginParam: Parameter[Set[Plugin]] = Parameter.mkNamed("plugin", Bukkit.getPluginManager.getPlugins)

}
