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

import java.util.Locale

import scala.language.higherKinds

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import cats.{Monad, MonadError, ~>}
import net.katsstuff.scammander
import shapeless._

trait ScammanderBase[F[_]] {

  implicit def F: MonadError[F, CommandFailureNEL]

  type SF[A] = StateT[F, List[RawCmdArg], A]
  def SF: Monad[SF] = Monad[SF]

  val FtoSF: F ~> SF = StateT.liftK[F, List[RawCmdArg]]

  type RootSender
  type RunExtra
  type TabExtra
  type Result
  type StaticChildCommand <: BaseStaticChildCommand

  protected val defaultCommandSuccess: Result

  protected def tabExtraToRunExtra(extra: TabExtra): RunExtra

  type BaseStaticChildCommand = scammander.ComplexBaseStaticChildCommand[
    F,
    RootSender,
    RunExtra,
    TabExtra,
    Result,
    StaticChildCommand
  ]

  type ChildCommand = scammander.ComplexChildCommand[F, StaticChildCommand]
  val ChildCommand: ComplexChildCommand.type = scammander.ComplexChildCommand

  type UserValidator[A] = ComplexUserValidator[F, A, RootSender]

  object UserValidator {
    def apply[A](implicit validator: UserValidator[A]): UserValidator[A] = validator

    /**
      * Create a user validator from a function.
      */
    //noinspection ConvertExpressionToSAM
    def mkValidator[A](validator: RootSender => F[A]): UserValidator[A] =
      new UserValidator[A] {
        override def validate(sender: RootSender): F[A] = validator(sender)
      }
  }

  implicit val rootValidator: UserValidator[RootSender] = UserValidator.mkValidator(F.pure)

  //Results and steps

  type CommandFailure    = scammander.CommandFailure
  type CommandFailureNEL = NonEmptyList[CommandFailure]

  type CommandSuccess = scammander.CommandSuccess[Result]
  val CommandSuccess: scammander.CommandSuccess.type = scammander.CommandSuccess

  type CommandError = scammander.CommandError
  val CommandError: scammander.CommandError.type = scammander.CommandError

  type CommandSyntaxError = scammander.CommandSyntaxError
  val CommandSyntaxError: scammander.CommandSyntaxError.type = scammander.CommandSyntaxError

  type CommandUsageError = scammander.CommandUsageError
  val CommandUsageError: scammander.CommandUsageError.type = scammander.CommandUsageError

  type HasName[A] = scammander.HasName[A]
  val HasName: scammander.HasName.type = scammander.HasName

  type RawCmdArg = scammander.RawCmdArg
  val RawCmdArg: scammander.RawCmdArg.type = scammander.RawCmdArg

  //Commands and parameters

  type ComplexCommand = scammander.ComplexCommand[F, RootSender, RunExtra, TabExtra, Result, StaticChildCommand]

  abstract class InnerCommand[Sender, Param](
      implicit
      userValidator: UserValidator[Sender],
      par: Parameter[Param]
  ) extends ComplexCommand {

    override def runRaw(source: RootSender, extra: RunExtra, args: List[RawCmdArg]): F[CommandSuccess] =
      for {
        sender <- userValidator.validate(source)
        param  <- par.parse(source, extra).runA(args)
        result <- run(sender, extra, param)
      } yield result

    def run(source: Sender, extra: RunExtra, arg: Param): F[CommandSuccess]

    override def suggestions(source: RootSender, extra: TabExtra, args: List[RawCmdArg]): F[Seq[String]] =
      par.suggestions(source, extra).runA(args)

    override def usage(source: RootSender): F[String] = par.usage(source)
  }

  type Command[Sender, Param] = InnerCommand[Sender, Param]

  object Command {

    /**
      * Create a simple command from a function that takes a parameter of the given type.
      */
    def simple[Param: Parameter](
        runCmd: (RootSender, RunExtra, Param) => F[CommandSuccess]
    ): Command[RootSender, Param] =
      new Command[RootSender, Param] {
        override def run(source: RootSender, extra: RunExtra, arg: Param): F[CommandSuccess] =
          runCmd(source, extra, arg)
      }

    /**
      * Create a command from a function that takes a parameter and sender of the given types.
      */
    def withSender[Sender: UserValidator, Param: Parameter](
        runCmd: (Sender, RunExtra, Param) => F[CommandSuccess]
    ): Command[Sender, Param] =
      new Command[Sender, Param] {
        override def run(source: Sender, extra: RunExtra, arg: Param): F[CommandSuccess] =
          runCmd(source, extra, arg)
      }

    /**
      * Create a simple command with children from a function that takes a
      * parameter of the given type.
      */
    def withChildren[Param: Parameter](childSet: Set[ChildCommand])(
        runCmd: (RootSender, RunExtra, Param) => F[CommandSuccess]
    ): Command[RootSender, Param] = new Command[RootSender, Param] {
      override def run(source: RootSender, extra: RunExtra, arg: Param): F[CommandSuccess] =
        runCmd(source, extra, arg)

      override def children: Set[ChildCommand] = childSet
    }

    /**
      * Create a command with children from a function that takes a parameter and sender of the given types.
      */
    def withSenderAndChildren[Sender: UserValidator, Param: Parameter](childSet: Set[ChildCommand])(
        runCmd: (Sender, RunExtra, Param) => F[CommandSuccess]
    ): Command[Sender, Param] =
      new Command[Sender, Param] {
        override def run(source: Sender, extra: RunExtra, arg: Param): F[CommandSuccess] =
          runCmd(source, extra, arg)

        override def children: Set[ChildCommand] = childSet
      }

    /**
      * Lifts an value F[A] into the parser State.
      */
    def liftFtoSF[A](fa: F[A]): SF[A] = FtoSF(fa)

    /**
      * Lifts an either value into the parser state.
      */
    def liftEitherToSF[A](value: Either[CommandFailureNEL, A]): SF[A] = FtoSF(F.fromEither(value))

    /**
      * Creates a command success step.
      */
    def successF(result: Result = defaultCommandSuccess): F[CommandSuccess] =
      F.pure(CommandSuccess(result))

    /**
      * Creates a generic command error step.
      */
    def errorState[A](msg: String, shouldShowUsage: Boolean = false): SF[A] =
      liftFtoSF(errorF(msg, shouldShowUsage))

    /**
      * Creates a syntax command error step.
      */
    def syntaxErrorState[A](msg: String, pos: Int): SF[A] =
      liftFtoSF(syntaxErrorF(msg, pos))

    /**
      * Creates a usage  command error step.
      */
    def usageErrorState[A](msg: String, pos: Int): SF[A] =
      liftFtoSF(usageErrorF(msg, pos))

    /**
      * Creates a generic command error step.
      */
    def errorF[A](msg: String, shouldShowUsage: Boolean = false): F[A] =
      F.raiseError(errorNel(msg, shouldShowUsage))

    /**
      * Creates a syntax command error step.
      */
    def syntaxErrorF[A](msg: String, pos: Int): F[A] =
      F.raiseError(syntaxErrorNel(msg, pos))

    /**
      * Creates a usage  command error step.
      */
    def usageErrorF[A](msg: String, pos: Int): F[A] =
      F.raiseError(usageErrorNel(msg, pos))

    /**
      * Creates a generic command error NEL.
      */
    def errorNel[A](msg: String, shouldShowUsage: Boolean = false): CommandFailureNEL =
      NonEmptyList.one(error(msg, shouldShowUsage))

    /**
      * Creates a syntax command error NEL.
      */
    def syntaxErrorNel[A](msg: String, pos: Int): NonEmptyList[CommandSyntaxError] =
      NonEmptyList.one(syntaxError(msg, pos))

    /**
      * Creates a usage  command error NEL.
      */
    def usageErrorNel[A](msg: String, pos: Int): NonEmptyList[CommandUsageError] =
      NonEmptyList.one(usageError(msg, pos))

    /**
      * Creates a command success.
      */
    def success(result: Result = defaultCommandSuccess): CommandSuccess = CommandSuccess(result)

    /**
      * Creates a generic command error.
      */
    def error(msg: String, shouldShowUsage: Boolean = false): CommandFailure = CommandError(msg, shouldShowUsage)

    /**
      * Creates a syntax command error.
      */
    def syntaxError(msg: String, pos: Int): CommandSyntaxError = CommandSyntaxError(msg, pos)

    /**
      * Creates a usage  command error.
      */
    def usageError(msg: String, pos: Int): CommandUsageError = CommandUsageError(msg, pos)
  }

  type Parameter[A] = InnerParameter[A]

  trait InnerParameter[A] extends scammander.ComplexParameter[F, A, RootSender, RunExtra, TabExtra] {

    /**
      * The usage for this command.
      */
    def usage(source: RootSender): F[String] = s"<$name>".pure
  }

  implicit class OptionOps[A](val option: Option[A]) {
    def toF(error: => String): F[A] =
      option.fold[F[A]](Command.errorF(error))(F.pure)
  }

  /**
    * Represents a parameter that wraps another parameter and transforms the parsed value.
    * @tparam A The new value
    * @tparam B The old value
    */
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] = param.suggestions(source, extra)

    override def usage(source: RootSender): F[String] = param.usage(source)
  }

  object Parameter {
    def apply[A](implicit param: Parameter[A]): Parameter[A] = param

    /**
      * Makes a parameter for a type which has names.
      * @param paramName The name of the parameter.
      * @param choices The possible choices for the parameter.
      * @tparam A The type of parameter.
      */
    def mkNamed[A: HasName](paramName: String, choices: => Iterable[A]): Parameter[Set[A]] = new Parameter[Set[A]] {
      override val name: String = paramName

      override def parse(source: RootSender, extra: RunExtra): SF[Set[A]] =
        ScammanderHelper.parseMany[F, A](name, choices)

      override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
        ScammanderHelper.suggestionsNamed(parse(source, tabExtraToRunExtra(extra)), choices)
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

      override def parse(source: RootSender, extra: RunExtra): SF[A] =
        ScammanderHelper.parse[F, A](choiceName, names.map(_ -> obj).toMap)

      override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), names)

      override def usage(source: RootSender): F[String] = name.pure
    }

    def choice[A](choiceName: String, choices: Map[String, A]): Parameter[Set[A]] = new Parameter[Set[A]] {

      override val name: String = choices.keys.mkString("|")

      override def parse(source: RootSender, extra: RunExtra): SF[Set[A]] =
        ScammanderHelper.parseMany[F, A](choiceName, choices)

      override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), choices.keys)

      override def usage(source: RootSender): F[String] = name.pure
    }
  }

  case class Named[Name <: String, A](value: A)
  implicit def namedParam[Name <: String, A](
      implicit paramParam: Parameter[A],
      witness: Witness.Aux[Name]
  ): Parameter[Named[Name, A]] = new ProxyParameter[Named[Name, A], A] {

    override val name: String = witness.value

    override def param: Parameter[A] = paramParam

    override def parse(source: RootSender, extra: RunExtra): SF[Named[Name, A]] =
      paramParam.parse(source, extra).map(Named.apply)
  }

  /**
    * Represents a named command. If one of these are in the implicit scope,
    * you can use a [[DynamicCommand]] in you command.
    * @param names The names for the command
    * @param command A function that returns the command itself
    * @param identifier A type level identifier for the command
    * @tparam Args The arguments needed to use this command
    * @tparam Identifier A type level identifier for the command
    * @tparam Sender The sender type of the command
    * @tparam Param The parameter type for the command
    */
  case class NamedCommand[Args, Identifier, Sender, Param](
      names: Seq[String],
      command: Args => Command[Sender, Param],
      identifier: Identifier
  )

  def dynamicCommandOf[Args, Identifier, Sender, Param](
      command: Args => Command[Sender, Param],
      identifier: Identifier
  ): GetDynamicCommandType[Args, Identifier, Sender, Param] = new GetDynamicCommandType[Args, Identifier, Sender, Param]

  class GetDynamicCommandType[Args, Identifier, Sender, Param] {
    type T = DynamicCommand[Args, Identifier, Sender, Param]
  }

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

      override def parse(
          source: RootSender,
          extra: RunExtra
      ): SF[DynamicCommand[Args, Identifier, Sender, Param]] =
        ScammanderHelper.parse(name, cmd.names.map(_ -> dynamic).toMap)

      override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), cmd.names)
    }

  implicit val rawCmdArgsParam: Parameter[List[RawCmdArg]] = new Parameter[List[RawCmdArg]] {

    override val name: String = "raw..."

    override def parse(source: RootSender, extra: RunExtra): SF[List[RawCmdArg]] =
      ScammanderHelper.getArgs

    override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
      StateT.set[F, List[RawCmdArg]](Nil) *> SF.pure(Nil)
  }

  trait NotUsed
  object NotUsed extends NotUsed
  implicit val notUsedParam: Parameter[NotUsed] = new Parameter[NotUsed] {

    override val name: String = ""

    override def parse(source: RootSender, extra: RunExtra): SF[NotUsed] =
      ScammanderHelper.firstArgOpt[F].flatMapF { arg =>
        if (arg.exists(_.content.nonEmpty)) Command.errorF("Too many arguments for command")
        else F.pure(NotUsed)
      }

    override def suggestions(source: RootSender, extra: TabExtra): SF[Seq[String]] =
      ScammanderHelper.dropFirstArg

    override def usage(source: RootSender): F[String] = "".pure
  }
}
