package net.katsstuff.scammander.bukkit

import scala.collection.JavaConverters._

import org.bukkit.command.{BlockCommandSender, CommandSender}
import org.bukkit.entity.{Entity, Player}
import org.bukkit.plugin.Plugin
import org.bukkit.util.{Vector => BukkitVector}
import org.bukkit.{Bukkit, OfflinePlayer, World}

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import net.katsstuff.scammander.{HelperParameters, NormalParameters, ScammanderBase, ScammanderHelper}
import net.katsstuff.scammander
import shapeless.Witness

trait BukkitParameters {
  self: ScammanderBase[Either[NonEmptyList[scammander.CommandFailure], ?], CommandSender, BukkitExtra, BukkitExtra]
    with NormalParameters[Either[NonEmptyList[scammander.CommandFailure], ?], CommandSender, BukkitExtra, BukkitExtra]
    with HelperParameters[
      Either[NonEmptyList[scammander.CommandFailure], ?],
      CommandSender,
      BukkitExtra,
      BukkitExtra
    ] =>

  implicit val playerHasName: HasName[Player]               = HasName.instance((a: Player) => a.getName)
  implicit val offlinePlayerHasName: HasName[OfflinePlayer] = HasName.instance((a: OfflinePlayer) => a.getName)
  implicit val worldHasName: HasName[World]                 = HasName.instance((a: World) => a.getName)
  implicit val pluginHasName: HasName[Plugin]               = HasName.instance((a: Plugin) => a.getName)

  type CommandStep[A] = Either[CommandFailureNEL, A]

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
      ): StateT[CommandStep, List[RawCmdArg], NeedPermission[S, A]] =
        if (source.hasPermission(perm))
          param.parse(source, extra).map(NeedPermission.apply)
        else
          ScammanderHelper.getPos.flatMapF { pos =>
            Command.usageErrorF("You do not have the permissions needed to use this parameter", pos)
          }

      override def suggestions(
          source: CommandSender,
          extra: BukkitExtra
      ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
        if (source.hasPermission(perm)) super.suggestions(source, extra)
        else ScammanderHelper.dropFirstArg[CommandStep]
    }

  //TODO: Selector with NMS
  implicit val allPlayerParam: Parameter[Set[Player]] = Parameter.mkNamed("player", Bukkit.getOnlinePlayers.asScala)

  implicit val playerParam: Parameter[Player] = new ProxyParameter[Player, OnlyOne[Player]] {
    override def param: Parameter[OnlyOne[Player]] = Parameter[OnlyOne[Player]]
    override def parse(source: CommandSender, extra: BukkitExtra): StateT[CommandStep, List[RawCmdArg], Player] =
      param.parse(source, extra).map(_.value)
  }

  //TODO: Entity selector with NMS

  implicit val offlinePlayerParam: Parameter[Set[OfflinePlayer]] = new Parameter[Set[OfflinePlayer]] {
    override def name: String = "offlinePlayer"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra
    ): StateT[CommandStep, List[RawCmdArg], Set[OfflinePlayer]] = {
      val players: StateT[CommandStep, List[RawCmdArg], Set[OfflinePlayer]] =
        allPlayerParam.parse(source, extra).map(_.map(p => p: OfflinePlayer))
      lazy val users: StateT[CommandStep, List[RawCmdArg], Set[OfflinePlayer]] =
        ScammanderHelper.parseMany(name, Bukkit.getOfflinePlayers)

      ScammanderHelper.withFallbackState(players, users)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] = {
      val parse = ScammanderHelper.firstArgAndDrop.flatMapF { arg =>
        val res = Bukkit.getOfflinePlayers.exists(obj => HasName(obj).equalsIgnoreCase(arg.content))
        if (res) true.pure else Command.errorF("Not parsed")
      }

      ScammanderHelper.suggestionsNamed(parse, Bukkit.getOfflinePlayers)
    }
  }

  implicit val worldParam: Parameter[Set[World]] = Parameter.mkNamed("world", Bukkit.getWorlds.asScala)

  implicit val vector3dParam: Parameter[BukkitVector] = new Parameter[BukkitVector] {
    override def name: String = "vector3d"

    override def parse(
        source: CommandSender,
        extra: BukkitExtra
    ): StateT[CommandStep, List[RawCmdArg], BukkitVector] = {
      val relative = source match {
        case entity: Entity                  => Some(entity.getLocation)
        case blockSender: BlockCommandSender => Some(blockSender.getBlock.getLocation)
        case _                               => None
      }

      for {
        x <- parseRelativeDouble(source, extra, relative.map(_.getX))
        y <- parseRelativeDouble(source, extra, relative.map(_.getY))
        z <- parseRelativeDouble(source, extra, relative.map(_.getZ))
      } yield new BukkitVector(x, y, z)
    }

    override def suggestions(
        source: CommandSender,
        extra: BukkitExtra
    ): StateT[CommandStep, List[RawCmdArg], Seq[String]] =
      ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg *> ScammanderHelper.dropFirstArg

    private def parseRelativeDouble(
        source: CommandSender,
        extra: BukkitExtra,
        relativeToOpt: Option[Double]
    ): StateT[CommandStep, List[RawCmdArg], Double] =
      for {
        arg <- ScammanderHelper.firstArg[CommandStep]
        res <- {
          if (arg.content.startsWith("~")) {
            Command
              .liftFtoSF(
                relativeToOpt.toRight(
                  Command.usageErrorNel("Relative position specified but source does not have a position", arg.start)
                )
              )
              .flatMap { relativeTo =>
                val newArg = arg.content.substring(1)
                if (newArg.isEmpty) SF.pure(relativeTo)
                else {
                  doubleParam
                    .parse(source, extra)
                    .contramap[List[RawCmdArg]](
                      xs => xs.headOption.fold(xs)(head => head.copy(content = newArg) :: xs.tail)
                    )
                    .map(_ + relativeTo)
                }
              }

          } else doubleParam.parse(source, extra)
        }
      } yield res
  }

  implicit val pluginParam: Parameter[Set[Plugin]] = Parameter.mkNamed("plugin", Bukkit.getPluginManager.getPlugins)

}
