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
package net.katsstuff.scammander

import scala.language.higherKinds

import java.util.Locale

import cats.syntax.all._
import cats.{Id => _, catsInstancesForId => _, _}
import net.katsstuff.scammander
import shapeless._

trait ScammanderBase extends ScammanderTypes {

  type RootSender
  type RunExtra
  type TabExtra
  type ResultTpe
  type StaticChildCommand <: BaseStaticChildCommand

  type G[_]

  protected val defaultCommandSuccess: ResultTpe

  protected def tabExtraToRunExtra(extra: TabExtra): RunExtra

  type Result = Either[CommandFailureNEL, CommandSuccess]

  type BaseStaticChildCommand = scammander.ComplexStaticChildCommand[
    G,
    RootSender,
    RunExtra,
    TabExtra,
    ResultTpe,
    StaticChildCommand
  ]

  type ChildCommand = scammander.ComplexChildCommand[StaticChildCommand]
  val ChildCommand: ComplexChildCommand.type = scammander.ComplexChildCommand

  type UserValidator[A] = ComplexUserValidator[A, RootSender]

  object UserValidator {
    def apply[A](implicit validator: UserValidator[A]): UserValidator[A] = validator
  }

  implicit def rootValidator: UserValidator[RootSender] = new UserValidator[RootSender] {
    override def validate[F[_]: ParserError](sender: RootSender): F[RootSender] = sender.pure
  }

  //Results and steps

  type CommandSuccess = scammander.CommandSuccess[ResultTpe]

  //Commands and parameters

  type ComplexCommand = scammander.ComplexCommand[G, RootSender, RunExtra, TabExtra, ResultTpe, StaticChildCommand]

  class Command[Sender, Param](
      runCmd: (Sender, RunExtra, Param) => G[CommandSuccess],
      childSet: Set[ChildCommand]
  )(implicit validator: UserValidator[Sender], param: Parameter[Param])
      extends ComplexCommand {

    override def runRaw[F[_]: ParserState: ParserError](
        source: RootSender,
        extra: RunExtra
    ): F[G[CommandSuccess]] =
      for {
        sender <- validator.validate(source)
        parsed <- param.parse(source, extra)
      } yield runCmd(sender, extra, parsed)

    override def suggestions[F[_]: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] = param.suggestions(source, extra)

    override def usage[F[_]: ParserError](source: RootSender): F[String] = param.usage(source)

    override def children: Set[ChildCommand] = childSet
  }

  protected def createCommand[Sender: UserValidator, Param: Parameter](
      runCmd: (Sender, RunExtra, Param) => G[CommandSuccess],
      childSet: Set[ChildCommand]
  ): Command[Sender, Param] = new Command[Sender, Param](runCmd, childSet)

  object Command {

    /**
      * Create a simple command from a function that takes a parameter of the given type.
      */
    def simple[Param: Parameter](
        runCmd: (RootSender, RunExtra, Param) => G[CommandSuccess]
    ): Command[RootSender, Param] = createCommand(runCmd, Set.empty)

    /**
      * Create a command from a function that takes a parameter and sender of the given types.
      */
    def withSender[Sender: UserValidator, Param: Parameter](
        runCmd: (Sender, RunExtra, Param) => G[CommandSuccess]
    ): Command[Sender, Param] = createCommand(runCmd, Set.empty)

    /**
      * Create a simple command with children from a function that takes a
      * parameter of the given type.
      */
    def withChildren[Param: Parameter](childSet: Set[ChildCommand])(
        runCmd: (RootSender, RunExtra, Param) => G[CommandSuccess]
    ): Command[RootSender, Param] = createCommand(runCmd, childSet)

    /**
      * Create a command with children from a function that takes a parameter and sender of the given types.
      */
    def withSenderAndChildren[Sender: UserValidator, Param: Parameter](childSet: Set[ChildCommand])(
        runCmd: (Sender, RunExtra, Param) => G[CommandSuccess]
    ): Command[Sender, Param] = createCommand(runCmd, childSet)
  }

  implicit class ResultExtra(private val result: Result.type) {

    /**
      * Creates a command success step.
      */
    def successF[F[_]: Applicative](result: ResultTpe = defaultCommandSuccess): F[CommandSuccess] =
      CommandSuccess(result).pure

    /**
      * Creates a command success.
      */
    def success(result: ResultTpe = defaultCommandSuccess): CommandSuccess = CommandSuccess(result)
  }

  trait Parameter[A] extends scammander.ComplexParameter[A, RootSender, RunExtra, TabExtra] {

    /**
      * The usage for this command.
      */
    def usage[F[_]: ParserError](source: RootSender): F[String] = s"<$name>".pure
  }

  /**
    * Represents a parameter that wraps another parameter and transforms the parsed value.
    * @tparam A The new value
    * @tparam B The old value
    */
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      param.suggestions(source, extra)

    override def usage[F[_]: ParserError](source: RootSender): F[String] = param.usage(source)
  }

  object Parameter {
    def apply[A](implicit param: Parameter[A]): Parameter[A] = param

    implicit def paramInstance: Functor[Parameter] = new Functor[Parameter] {

      override def map[A, B](fa: Parameter[A])(f: A => B): Parameter[B] = new ProxyParameter[B, A] {
        override def param: Parameter[A] = fa
        override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[B] =
          param.parse(source, extra).map(f)
      }
    }

    /**
      * Makes a parameter for a type which has names.
      * @param paramName The name of the parameter.
      * @param choices The possible choices for the parameter.
      * @tparam A The type of parameter.
      */
    def mkNamed[A: HasName](
        paramName: String,
        choices: => Iterable[A]
    ): Parameter[Set[A]] = new Parameter[Set[A]] {
      override val name: String = paramName

      override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[Set[A]] =
        ScammanderHelper.parseMany(name, choices)

      override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
        ScammanderHelper.suggestionsNamed(parse(source, tabExtraToRunExtra(extra)), Eval.always(choices))
    }

    def mkSingleton[A](obj: A)(implicit typeable: Typeable[A]): Parameter[A] = {
      val name =
        if (typeable.describe.endsWith(".type")) typeable.describe.dropRight(5).toLowerCase(Locale.ROOT)
        else typeable.describe.toLowerCase(Locale.ROOT)
      mkSingleton(obj, name, Seq(name))
    }

    def mkSingleton[A](obj: A, choiceName: String, names: Seq[String]): Parameter[A] = new Parameter[A] {

      override val name: String =
        names.mkString("|")

      override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[A] =
        ScammanderHelper.parse(choiceName, names.map(_ -> obj).toMap)

      override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(names))

      override def usage[F[_]: ParserError](source: RootSender): F[String] = name.pure
    }

    def choice[A](choiceName: String, choices: Map[String, A]): Parameter[Set[A]] = new Parameter[Set[A]] {

      override val name: String = choices.keys.mkString("|")

      override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[Set[A]] =
        ScammanderHelper.parseMany[F, A](choiceName, choices)

      override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(choices.keys))

      override def usage[F[_]: ParserError](source: RootSender): F[String] = name.pure
    }
  }

  case class Named[Name <: String, A](value: A)
  implicit def namedParam[Name <: String, A](
      implicit paramParam: Parameter[A],
      witness: Witness.Aux[Name]
  ): Parameter[Named[Name, A]] = new ProxyParameter[Named[Name, A], A] {

    override val name: String = witness.value

    override def param: Parameter[A] = paramParam

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[Named[Name, A]] =
      paramParam.parse(source, extra).map(Named.apply)
  }

  /**
    * Represents a named command. If one of these are in the implicit scope,
    * you can use a [[DynamicCommand]] in you command.
    * @param names The names for the command. If this represents a command tree,
    *              then these are the names of all the commands in the current level.
    * @param command A function that returns the command itself.
    * @param identifier A type level identifier for the command.
    * @tparam Args The arguments needed to use this command.
    * @tparam Identifier A type level identifier for the command.
    * @tparam Sender The sender type of the command.
    * @tparam Param The parameter type for the command.
    */
  case class NamedCommand[Args, Identifier, Sender, Param](
      names: Seq[String],
      command: Args => Command[Sender, Param],
      identifier: Identifier
  )

  /**
    * Use this method to get the type T which represents a dynamic command.
    * @param command The function which gives back a command.
    * @param identifier The identifier for the command.
    */
  def dynamicCommandOf[Args, Identifier, Sender, Param](
      command: Args => Command[Sender, Param],
      identifier: Identifier
  ): GetDynamicCommandType[Args, Identifier, Sender, Param] = {
    identity(command)
    identity(identifier)
    new GetDynamicCommandType[Args, Identifier, Sender, Param]
  }

  class GetDynamicCommandType[Args, Identifier, Sender, Param] {
    type T = DynamicCommand[Args, Identifier, Sender, Param]
  }

  /**
    * Like [[NamedCommand]] but parsed and includes a validator and parameter.
    */
  case class DynamicCommand[Args, Identifier, Sender, Param](
      names: Seq[String],
      command: Args => Command[Sender, Param],
      identifier: Identifier,
      validator: UserValidator[Sender],
      parameter: Parameter[Param]
  )

  implicit def dynamicCommandParameter[Args, Identifier, Sender, Param](
      implicit cmd: NamedCommand[Args, Identifier, Sender, Param],
      validator: UserValidator[Sender],
      parameter: Parameter[Param]
  ): Parameter[DynamicCommand[Args, Identifier, Sender, Param]] =
    new Parameter[DynamicCommand[Args, Identifier, Sender, Param]] {
      private val dynamic = DynamicCommand(cmd.names, cmd.command, cmd.identifier, validator, parameter)

      override val name: String = cmd.names.mkString("|")

      override def parse[F[_]: ParserState: ParserError](
          source: RootSender,
          extra: RunExtra
      ): F[DynamicCommand[Args, Identifier, Sender, Param]] =
        ScammanderHelper.parse(name, cmd.names.map(_ -> dynamic).toMap)

      override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(cmd.names))
    }

  implicit val rawCmdArgsParam: Parameter[List[RawCmdArg]] = new Parameter[List[RawCmdArg]] {

    override val name: String = "raw..."

    override def parse[F[_]](
        source: RootSender,
        extra: RunExtra
    )(implicit S: ParserState[F], E: ParserError[F]): F[List[RawCmdArg]] = S.get

    override def suggestions[F[_]](
        source: RootSender,
        extra: TabExtra
    )(implicit S: ParserState[F], E: ParserError[F]): F[Seq[String]] =
      S.set(Nil).as(Nil)
  }

  trait NotUsed
  object NotUsed extends NotUsed
  implicit val notUsedParam: Parameter[NotUsed] = new Parameter[NotUsed] {

    override val name: String = ""

    override def parse[F[_]: ParserState: ParserError](source: RootSender, extra: RunExtra): F[NotUsed] =
      ScammanderHelper.firstArgOpt.flatMap { arg =>
        if (arg.exists(_.content.nonEmpty)) Result.errorF[F, NotUsed]("Too many arguments for command")
        else (NotUsed: NotUsed).pure
      }

    override def suggestions[F[_]: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]] =
      ScammanderHelper.dropFirstArg

    override def usage[F[_]: ParserError](source: RootSender): F[String] = "".pure
  }
}
