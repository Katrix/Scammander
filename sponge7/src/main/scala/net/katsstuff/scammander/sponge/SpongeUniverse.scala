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

import java.util
import java.util.Optional

import scala.collection.JavaConverters._

import org.spongepowered.api.Sponge
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.command.{CommandCallable, CommandException, CommandMapping, CommandResult, CommandSource}
import org.spongepowered.api.entity.living.player.Player
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

import net.katsstuff.scammander.misc.RawCmdArg
import net.katsstuff.scammander.{ScammanderHelper, ScammanderUniverse}
import shapeless._

trait SpongeUniverse extends ScammanderUniverse[CommandSource, Unit, Location[World]] {

  case class NeedPermission[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[A, A] {
    val perm: String = w.value

    override def parse(
        source: CommandSource,
        extra: Unit,
        xs: List[RawCmdArg]
    ): Either[CmdFailure, (List[RawCmdArg], A)] =
      if (source.hasPermission(perm)) param.parse(source, extra, xs)
      else
        Left(
          CmdUsageError(
            "You do not have the permissions needed to use this parameter",
            xs.headOption.map(_.start).getOrElse(-1)
          )
        )
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
    case player: Player => Right(player)
    case _              => Left(CmdUsageError("This command can only be used by players", -1))
  }(identity)

  case class SpongeCommandWrapper[Sender, Param](command: Command[Sender, Param], info: CommandInfo)
      extends CommandCallable {

    override def process(source: CommandSource, arguments: String): CommandResult = {
      val res = for {
        sender <- command.userValidator.validate(source)
        param  <- command.par.parse(source, (), ScammanderHelper.stringToRawArgs(arguments))
      } yield command.run(sender, (), param._2)

      res.merge match {
        case CmdSuccess(count) => CommandResult.successCount(count)
        case CmdError(msg)     => throw new CommandException(Text.of(msg))
        case CmdSyntaxError(msg, pos) =>
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case CmdUsageError(msg, pos) =>
          //TODO: Custom exception
          val e =
            if (pos != -1) new ArgumentParseException(Text.of(msg), arguments, pos)
            else new CommandException(Text.of(msg))
          throw e
        case e: MultipleCmdErrors => throw new CommandException(Text.of(e.msg)) //TODO: Better error here
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
