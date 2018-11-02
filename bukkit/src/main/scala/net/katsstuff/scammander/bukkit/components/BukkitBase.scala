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
package net.katsstuff.scammander.bukkit.components

import scala.collection.JavaConverters._

import cats.Applicative
import cats.arrow.FunctionK
import cats.syntax.all._
import net.katsstuff.scammander.ScammanderBase
import org.bukkit.command.CommandSender
import org.bukkit.plugin.java.JavaPlugin

trait BukkitBase extends ScammanderBase {

  override type RootSender         = CommandSender
  override type RunExtra           = BukkitExtra
  override type TabExtra           = BukkitExtra
  override type ResultTpe          = Boolean
  override type StaticChildCommand = ChildCommandExtra[G]

  implicit protected def G: Applicative[G]

  override protected val defaultCommandSuccess: Boolean = true

  protected def runG[A](computation: G[A]): Either[CommandFailureNEL, A]

  override protected def tabExtraToRunExtra(extra: BukkitExtra): BukkitExtra = extra

  /**
    * Helper for creating an alias when registering a command.
    */
  object Alias {
    def apply(first: String, aliases: String*): Set[String] = aliases.toSet + first
  }

  /**
    * Helper for creating a alias when registering a command.
    */
  object Permission {
    def apply(perm: String): Some[String] = Some(perm)
    val none: None.type                   = None
  }

  /**
    * Helper for creating a help when registering a command.
    */
  object Help {
    def liftF(f: CommandSender => G[String]): CommandSender => G[Option[String]] = f.andThen(_.map(Some.apply))
    def apply(f: CommandSender => String): CommandSender => G[Option[String]]    = f.andThen(text => G.pure(Some(text)))
    def apply(text: String): CommandSender => G[Option[String]]                  = _ => G.pure(Some(text))
    val none: CommandSender => G[Option[String]]                                 = _ => G.pure(None)
  }

  /**
    * Helper for creating an description when registering a command.
    */
  object Description {
    def liftF(f: CommandSender => G[String]): CommandSender => G[Option[String]] = f.andThen(_.map(Some.apply))
    def apply(f: CommandSender => String): CommandSender => G[Option[String]]    = f.andThen(text => G.pure(Some(text)))
    def apply(text: String): CommandSender => G[Option[String]]                  = _ => G.pure(Some(text))
    val none: CommandSender => G[Option[String]]                                 = _ => G.pure(None)
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toBukkit: BukkitCommandWrapper[G] =
      BukkitCommandWrapper(command, λ[FunctionK[G, Either[CommandFailureNEL, ?]]](runG(_)))
    def toChild(
        aliases: Set[String],
        permission: Option[String] = Permission.none,
        help: CommandSender => G[Option[String]] = Help.none,
        description: CommandSender => G[Option[String]] = Description.none
    ): ChildCommand =
      ChildCommand(aliases, ChildCommandExtra(command.toBukkit, permission, help, description))

    def toRootChild(plugin: JavaPlugin, name: String): ChildCommand = {
      val bukkitCommand = plugin.getCommand(name)

      ChildCommand(
        bukkitCommand.getAliases.asScala.toSet + bukkitCommand.getName,
        ChildCommandExtra(
          toBukkit,
          Option(bukkitCommand.getPermission),
          Help.none,
          if (bukkitCommand.getDescription.isEmpty) Description.none else Description(bukkitCommand.getDescription)
        )
      )
    }

    def register(plugin: JavaPlugin, name: String): Unit = toBukkit.register(plugin, name)
  }
}
