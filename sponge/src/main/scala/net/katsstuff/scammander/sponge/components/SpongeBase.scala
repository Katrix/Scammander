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
package net.katsstuff.scammander.sponge.components

import cats.arrow.FunctionK
import cats.mtl.{ApplicativeHandle, DefaultApplicativeHandle}
import cats.{Applicative, Functor, MonadError}
import net.katsstuff.scammander.ScammanderBase
import org.spongepowered.api.command._
import org.spongepowered.api.text.Text
import org.spongepowered.api.world.{Location, World}

trait SpongeBase extends ScammanderBase {

  override type RootSender         = CommandSource
  override type RunExtra           = Unit
  override type TabExtra           = Option[Location[World]]
  override type ResultTpe          = Int
  override type StaticChildCommand = SpongeCommandWrapper[G]

  protected def G: MonadError[G, CommandFailureNEL]
  def GE: ApplicativeHandle[G, CommandFailureNEL] = new DefaultApplicativeHandle[G, CommandFailureNEL] {
    override val functor: Functor[G]                                         = G
    override val applicative: Applicative[G]                                 = G
    override def handleWith[A](fa: G[A])(f: CommandFailureNEL => G[A]): G[A] = G.handleErrorWith(fa)(f)
    override def raise[A](e: CommandFailureNEL): G[A]                        = G.raiseError(e)
  }

  override protected val defaultCommandSuccess: Int = 1

  protected def runG[A](computation: G[A]): Either[CommandFailureNEL, A]

  override protected def tabExtraToRunExtra(extra: Option[Location[World]]): Unit = ()

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
    val none: None.type                   = None
  }

  /**
    * Helper for creating a help when registering a command.
    */
  object Help {
    def liftF(f: CommandSource => G[Text]): CommandSource => G[Option[Text]] = f.andThen(G.map(_)(Some.apply))
    def apply(f: CommandSource => Text): CommandSource => G[Option[Text]]    = f.andThen(text => G.pure(Some(text)))
    def apply(text: Text): CommandSource => G[Option[Text]]                  = _ => G.pure(Some(text))
    val none: CommandSource => G[Option[Text]]                               = _ => G.pure(None)
  }

  /**
    * Helper for creating an description when registering a command.
    */
  object Description {
    def liftF(f: CommandSource => G[Text]): CommandSource => G[Option[Text]] = f.andThen(G.map(_)(Some.apply))
    def apply(f: CommandSource => Text): CommandSource => G[Option[Text]]    = f.andThen(text => G.pure(Some(text)))
    def apply(text: Text): CommandSource => G[Option[Text]]                  = _ => G.pure(Some(text))
    val none: CommandSource => G[Option[Text]]                               = _ => G.pure(None)
  }

  implicit class RichCommand[Sender, Param](val command: Command[Sender, Param]) {
    def toSponge(info: CommandInfo[G]): SpongeCommandWrapper[G] =
      SpongeCommandWrapper(command, info, λ[FunctionK[G, Either[CommandFailureNEL, ?]]](runG(_)))
    def toChild(aliases: Seq[String], info: CommandInfo[G]): ChildCommand =
      ChildCommand(aliases.toSet, toSponge(info))
    def toChild(
        aliases: Seq[String],
        permission: Option[String] = Permission.none,
        help: CommandSource => G[Option[Text]] = Help.none,
        shortDescription: CommandSource => G[Option[Text]] = Description.none
    ): ChildCommand =
      ChildCommand(aliases.toSet, toSponge(CommandInfo(permission, help, shortDescription)))

    def register(
        plugin: AnyRef,
        aliases: Seq[String],
        permission: Option[String] = Permission.none,
        help: CommandSource => G[Option[Text]] = Help.none,
        shortDescription: CommandSource => G[Option[Text]] = Description.none
    ): Option[CommandMapping] =
      toSponge(CommandInfo(permission, help, shortDescription)).register(plugin, aliases)
  }
}
