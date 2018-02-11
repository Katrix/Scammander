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
import org.spongepowered.api.command._
import org.spongepowered.api.command.args.ArgumentParseException
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

import net.katsstuff.scammander.{ScammanderBase, ScammanderHelper}

trait SpongeBase extends ScammanderBase[CommandSource, Unit, Location[World]] {

  override protected type Result                            = Int
  override protected type StaticChildCommand[Sender, Param] = SpongeCommandWrapper[Sender, Param]

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

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toSponge(info: CommandInfo): SpongeCommandWrapper[Sender, Param] = SpongeCommandWrapper(command, info)
    def toChild(aliases: Seq[String], info: CommandInfo): ChildCommand[Sender, Param] =
      ChildCommand(aliases.toSet, toSponge(info))
    def toChild(
        aliases: Seq[String],
        permission: Option[String] = None,
        help: CommandSource => Option[Text] = _ => None,
        shortDescription: CommandSource => Option[Text] = _ => None
    ): ChildCommand[Sender, Param] =
      ChildCommand(aliases.toSet, toSponge(CommandInfo(permission, help, shortDescription)))

    def register(
        plugin: AnyRef,
        aliases: Seq[String],
        permission: Option[String] = None,
        help: CommandSource => Option[Text] = _ => None,
        shortDescription: CommandSource => Option[Text] = _ => None
    ): Option[CommandMapping] =
      toSponge(CommandInfo(permission, help, shortDescription)).register(plugin, aliases)
  }

  case class SpongeCommandWrapper[Sender, Param](command: Command[Sender, Param], info: CommandInfo)
      extends CommandCallable
      with SharedStaticChildCommand[Sender, Param] {

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
      val args                   = ScammanderHelper.stringToRawArgsQuoted(arguments)
      lazy val content           = args.head.content
      lazy val childCommand      = command.childrenMap(content)
      def headCount(arg: String) = command.children.flatMap(_.aliases).count(_.startsWith(arg))
      val doChildCommand = if (args.nonEmpty && command.childrenMap.contains(content)) {
        if (headCount(content) > 1) args.lengthCompare(1) > 0 else true
      } else false

      if (doChildCommand && childCommand.testPermission(source)) {
        childCommand.getSuggestions(source, args.tail.map(_.content).mkString(" "), targetPosition)
      } else {
        val parse: List[RawCmdArg] => CommandStep[(List[RawCmdArg], Boolean)] = xs => {
          val isParsed = xs.headOption.exists { arg =>
            if (command.childrenMap.contains(arg.content) && headCount(arg.content) > 1) false
            else command.childrenMap.keys.exists(_.equalsIgnoreCase(arg.content))
          }
          Either.cond(isParsed, (xs.drop(1), true), Command.error("Not child"))
        }
        val childSuggestions = ScammanderHelper.suggestions(parse, args, command.childrenMap.keys)
        val paramSuggestions = command.suggestions(source, targetPosition, args)
        val ret = childSuggestions match {
          case Right(suggestions) => suggestions ++ paramSuggestions
          case Left(_)            => paramSuggestions
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
            Text.of(aliasPart, usagePart)
        }

        val childUsage = childUsages.mkString("|")
        Text.of(childUsage, "|", command.usage(source))
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
