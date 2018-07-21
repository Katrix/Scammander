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
import cats._
import net.katsstuff.scammander
import shapeless._

trait ScammanderBase[F[_]] {

  implicit def F: MonadError[F, CommandFailureNEL]

  type Parser[A] = StateT[F, List[RawCmdArg], A]
  def parser: Monad[Parser] = Monad[Parser]

  val FtoParser: F ~> Parser = StateT.liftK[F, List[RawCmdArg]]

  type RootSender
  type RunExtra
  type TabExtra
  type Result
  type StaticChildCommand <: BaseStaticChildCommand

  protected val defaultCommandSuccess: Result

  protected def tabExtraToRunExtra(extra: TabExtra): RunExtra

  type BaseStaticChildCommand = scammander.ComplexStaticChildCommand[
    F,
    RootSender,
    RunExtra,
    TabExtra,
    Result,
    StaticChildCommand
  ]

  type ChildCommand = scammander.ComplexChildCommand[StaticChildCommand]
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

  abstract class ParameterCommand[Sender, Param](
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

  type Command[Sender, Param] = ParameterCommand[Sender, Param]

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
    def liftFtoParser[A](fa: F[A]): Parser[A] = FtoParser(fa)

    /**
      * Lifts an either value into the parser state.
      */
    def liftEitherToParser[A](value: Either[CommandFailureNEL, A]): Parser[A] = FtoParser(F.fromEither(value))

    /**
      * Creates a command success step.
      */
    def successF(result: Result = defaultCommandSuccess): F[CommandSuccess] =
      F.pure(CommandSuccess(result))

    /**
      * Creates a generic command error step.
      */
    def errorParser[A](msg: String, shouldShowUsage: Boolean = false): Parser[A] =
      liftFtoParser(errorF(msg, shouldShowUsage))

    /**
      * Creates a syntax command error step.
      */
    def syntaxErrorParser[A](msg: String, pos: Int): Parser[A] =
      liftFtoParser(syntaxErrorF(msg, pos))

    /**
      * Creates a usage  command error step.
      */
    def usageErrorParser[A](msg: String, pos: Int): Parser[A] =
      liftFtoParser(usageErrorF(msg, pos))

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

  trait Parameter[A] extends scammander.ComplexParameter[F, A, RootSender, RunExtra, TabExtra] {

    /**
      * The usage for this command.
      */
    def usage(source: RootSender): F[String] = s"<$name>".pure[F]
  }

  implicit class OptionOps[A](val option: Option[A]) {
    def toF(error: => String): F[A] =
      F.fromOption(option, Command.errorNel(error))

    def toFLift(error: => F[String]): F[A] =
      option.fold[F[A]](error.flatMap(Command.errorF(_)))(F.pure)
  }

  /**
    * Represents a parameter that wraps another parameter and transforms the parsed value.
    * @tparam A The new value
    * @tparam B The old value
    */
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] = param.suggestions(source, extra)

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

      override def parse(source: RootSender, extra: RunExtra): Parser[Set[A]] =
        ScammanderHelper.parseMany[F, A](name, choices)

      override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
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

      override def parse(source: RootSender, extra: RunExtra): Parser[A] =
        ScammanderHelper.parse[F, A](choiceName, names.map(_ -> obj).toMap)

      override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(names))

      override def usage(source: RootSender): F[String] = name.pure[F]
    }

    def choice[A](choiceName: String, choices: Map[String, A]): Parameter[Set[A]] = new Parameter[Set[A]] {

      override val name: String = choices.keys.mkString("|")

      override def parse(source: RootSender, extra: RunExtra): Parser[Set[A]] =
        ScammanderHelper.parseMany[F, A](choiceName, choices)

      override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(choices.keys))

      override def usage(source: RootSender): F[String] = name.pure[F]
    }
  }

  case class Named[Name <: String, A](value: A)
  implicit def namedParam[Name <: String, A](
      implicit paramParam: Parameter[A],
      witness: Witness.Aux[Name]
  ): Parameter[Named[Name, A]] = new ProxyParameter[Named[Name, A], A] {

    override val name: String = witness.value

    override def param: Parameter[A] = paramParam

    override def parse(source: RootSender, extra: RunExtra): Parser[Named[Name, A]] =
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
  ): GetDynamicCommandType[Args, Identifier, Sender, Param] = new GetDynamicCommandType[Args, Identifier, Sender, Param]

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

      override def parse(
          source: RootSender,
          extra: RunExtra
      ): Parser[DynamicCommand[Args, Identifier, Sender, Param]] =
        ScammanderHelper.parse(name, cmd.names.map(_ -> dynamic).toMap)

      override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
        ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(cmd.names))
    }

  /**
    * Like [[NamedCommand]] but can be used with [[ComplexCommand]]s, and
    * completely hands over the execution. This parameter does not consume arguments.
    * @param names The names for the command. If this represents a command tree,
    *              then these are the names of all the commands in the current level.
    * @param command A function that returns the command itself.
    * @param identifier A type level identifier for the command.
    * @tparam Args The arguments needed to use this command.
    * @tparam Identifier A type level identifier for the command.
    */
  case class HandoverComplexCommandExecution[Args, Identifier](
      names: Seq[String],
      command: Args => ComplexCommand,
      identifier: Identifier
  )

  /**
    * Represents the result of executing a command with [[HandoverComplexCommandExecution]].
    * Use this as you parameter type.
    * @tparam Identifier The identifier for the command.
    */
  case class ComplexCommandExecution[Identifier](result: CommandSuccess)

  implicit def handoverComplexCommandExecutionParameter[Args, Identifier](
      implicit cmd: HandoverComplexCommandExecution[Args, Identifier],
      parameter: Parameter[Args]
  ): Parameter[ComplexCommandExecution[Identifier]] = new Parameter[ComplexCommandExecution[Identifier]] {

    override val name: String = cmd.names.mkString("|")

    override def parse(source: RootSender, extra: RunExtra): Parser[ComplexCommandExecution[Identifier]] =
      for {
        args    <- parameter.parse(source, extra)
        parsed  <- ScammanderHelper.parse(name, cmd.names.map(_ -> cmd).toMap)
        rawArgs <- ScammanderHelper.getArgs[F]
        runF = parsed.command(args).runRaw(source, extra, rawArgs)
        res <- Command.liftFtoParser(runF)
      } yield ComplexCommandExecution[Identifier](res)

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] = {
      val commandSuggestions = for {
        args    <- parameter.parse(source, tabExtraToRunExtra(extra))
        parsed  <- ScammanderHelper.parse(name, cmd.names.map(_ -> cmd).toMap)
        rawArgs <- ScammanderHelper.getArgs[F]
        suggestionsF = parsed.command(args).suggestions(source, extra, rawArgs)
        res <- Command.liftFtoParser(suggestionsF)
      } yield res

      ScammanderHelper.withFallbackParser(
        commandSuggestions,
        ScammanderHelper.withFallbackSuggestions(
          parameter.suggestions(source, extra),
          ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(cmd.names))
        )
      )
    }
  }

  /**
    * Executes a command like [[HandoverComplexCommandExecution]], but behaves move like [[Named]].
    * This parameter consume arguments. Think of it like a type level <* operation
    *
    * @param names The names for the command. If this represents a command tree,
    *              then these are the names of all the commands in the current level.
    * @param command A function that returns the command itself.
    * @param identifier A type level identifier for the command.
    * @tparam Args The arguments needed to use this command.
    * @tparam Identifier A type level identifier for the command.
    * @tparam Sender The sender type of the command.
    * @tparam Param The parameter type for the command.
    */
  case class HandoverCommandExecution[Args, Identifier, Sender, Param](
      names: Seq[String],
      command: Args => Command[Sender, Param],
      identifier: Identifier
  )

  /**
    * Represents the result of executing a command with [[HandoverCommandExecution]].
    * Use this as you parameter type.
    * @tparam Identifier The identifier for the command.
    */
  case class CommandExecution[Identifier](result: CommandSuccess)

  implicit def handoverCommandExecutionParameter[Args, Identifier, Sender, Param](
      implicit cmd: HandoverCommandExecution[Args, Identifier, Sender, Param],
      cmdArgsParameter: Parameter[Args],
      validator: UserValidator[Sender],
      cmdParamParameter: Parameter[Param]
  ): Parameter[CommandExecution[Identifier]] = new Parameter[CommandExecution[Identifier]] {

    override val name: String = cmd.names.mkString("|")

    override def parse(source: RootSender, extra: RunExtra): Parser[CommandExecution[Identifier]] =
      for {
        args     <- cmdArgsParameter.parse(source, extra)
        parsed   <- ScammanderHelper.parse(name, cmd.names.map(_ -> cmd).toMap)
        cmdParam <- cmdParamParameter.parse(source, extra)
        sender   <- Command.liftFtoParser(validator.validate(source))
        runF = parsed.command(args).run(sender, extra, cmdParam)
        res <- Command.liftFtoParser(runF)
      } yield CommandExecution[Identifier](res)

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
      ScammanderHelper.withFallbackSuggestions(
        ScammanderHelper.withFallbackSuggestions(
          cmdArgsParameter.suggestions(source, extra),
          ScammanderHelper.suggestions(parse(source, tabExtraToRunExtra(extra)), Eval.now(cmd.names))
        ),
        cmdParamParameter.suggestions(source, extra)
      )
  }

  implicit val rawCmdArgsParam: Parameter[List[RawCmdArg]] = new Parameter[List[RawCmdArg]] {

    override val name: String = "raw..."

    override def parse(source: RootSender, extra: RunExtra): Parser[List[RawCmdArg]] =
      ScammanderHelper.getArgs

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
      StateT.set[F, List[RawCmdArg]](Nil) *> parser.pure(Nil)
  }

  trait NotUsed
  object NotUsed extends NotUsed
  implicit val notUsedParam: Parameter[NotUsed] = new Parameter[NotUsed] {

    override val name: String = ""

    override def parse(source: RootSender, extra: RunExtra): Parser[NotUsed] =
      ScammanderHelper.firstArgOpt[F].flatMapF { arg =>
        if (arg.exists(_.content.nonEmpty)) Command.errorF("Too many arguments for command")
        else F.pure(NotUsed)
      }

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
      ScammanderHelper.dropFirstArg

    override def usage(source: RootSender): F[String] = "".pure[F]
  }
}
