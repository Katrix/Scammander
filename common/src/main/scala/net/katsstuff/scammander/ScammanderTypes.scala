package net.katsstuff.scammander

import scala.language.higherKinds

import cats.MonadError
import cats.data.NonEmptyList
import cats.mtl.MonadState
import cats.syntax.all._
import net.katsstuff.scammander

trait ScammanderTypes {

  type CommandFailure    = scammander.CommandFailure
  type CommandFailureNEL = NonEmptyList[CommandFailure]

  type HasName[A] = scammander.HasName[A]
  val HasName: scammander.HasName.type = scammander.HasName

  type RawCmdArg = scammander.RawCmdArg
  val RawCmdArg: scammander.RawCmdArg.type = scammander.RawCmdArg

  type ParserState[F[_]] = MonadState[F, List[RawCmdArg]]
  type ParserError[F[_]] = MonadError[F, CommandFailureNEL]

  object Result {

    /**
      * Creates a generic command error step.
      */
    def errorF[F[_]: ParserError, A](msg: String, shouldShowUsage: Boolean = false): F[A] =
      errorNel(msg, shouldShowUsage).raiseError

    /**
      * Creates a syntax command error step.
      */
    def syntaxErrorF[F[_]: ParserError, A](msg: String, pos: Int): F[A] =
      syntaxErrorNel(msg, pos).raiseError

    /**
      * Creates a usage  command error step.
      */
    def usageErrorF[F[_]: ParserError, A](msg: String, pos: Int): F[A] =
      usageErrorNel(msg, pos).raiseError

    /**
      * Creates a generic command error NEL.
      */
    def errorNel[A](msg: String, shouldShowUsage: Boolean = false): CommandFailureNEL =
      NonEmptyList.one(error(msg, shouldShowUsage))

    /**
      * Creates a syntax command error NEL.
      */
    def syntaxErrorNel[A](msg: String, pos: Int): CommandFailureNEL =
      NonEmptyList.one(syntaxError(msg, pos))

    /**
      * Creates a usage  command error NEL.
      */
    def usageErrorNel[A](msg: String, pos: Int): CommandFailureNEL =
      NonEmptyList.one(usageError(msg, pos))

    /**
      * Creates a generic command error.
      */
    def error(msg: String, shouldShowUsage: Boolean = false): CommandFailure = CommandError(msg, shouldShowUsage)

    /**
      * Creates a syntax command error.
      */
    def syntaxError(msg: String, pos: Int): CommandFailure = CommandSyntaxError(msg, pos)

    /**
      * Creates a usage  command error.
      */
    def usageError(msg: String, pos: Int): CommandFailure = CommandUsageError(msg, pos)
  }
}
object ScammanderTypes extends ScammanderTypes
