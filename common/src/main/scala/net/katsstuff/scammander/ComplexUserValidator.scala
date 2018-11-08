package net.katsstuff.scammander

import scala.language.higherKinds

import cats.mtl.syntax.all._
import cats.syntax.all._
import cats.{Monad, MonadError}
import net.katsstuff.scammander.ScammanderTypes.{CommandFailureNEL, ParserError}

/**
  * A typeclass which helps convert a user into another type.
  */
trait ComplexUserValidator[A, RootSender] {

  /**
    * Validates the sender.
    */
  def validate[F[_]: Monad: ParserError](sender: RootSender): F[A]
}
object ComplexUserValidator {
  def apply[A, RootSender](
      implicit validator: ComplexUserValidator[A, RootSender]
  ): ComplexUserValidator[A, RootSender] = validator

  implicit def instanceForValidator[RootSender]: MonadError[ComplexUserValidator[?, RootSender], CommandFailureNEL] = {
    type UserValidator[A] = ComplexUserValidator[A, RootSender]

    new MonadError[UserValidator, CommandFailureNEL] {
      override def map[A, B](fa: UserValidator[A])(
          f: A => B
      ): UserValidator[B] = new UserValidator[B] {
        override def validate[F[_]: Monad: ParserError](sender: RootSender): F[B] = fa.validate(sender).map(f)
      }

      override def flatMap[A, B](fa: UserValidator[A])(
          f: A => UserValidator[B]
      ): UserValidator[B] = new UserValidator[B] {
        override def validate[F[_]: Monad: ParserError](sender: RootSender): F[B] =
          fa.validate(sender).flatMap(a => f(a).validate(sender))
      }

      override def tailRecM[A, B](a: A)(
          f: A => UserValidator[Either[A, B]]
      ): UserValidator[B] = new UserValidator[B] {
        override def validate[F[_]: Monad: ParserError](sender: RootSender): F[B] =
          Monad[F].tailRecM(a)(a => f(a).validate(sender))
      }

      override def raiseError[A](
          e: CommandFailureNEL
      ): UserValidator[A] = new UserValidator[A] {
        override def validate[F[_]: Monad: ParserError](sender: RootSender): F[A] = e.raise
      }

      override def handleErrorWith[A](fa: UserValidator[A])(
          f: CommandFailureNEL => UserValidator[A]
      ): UserValidator[A] = new UserValidator[A] {
        override def validate[F[_]: Monad](sender: RootSender)(implicit E: ParserError[F]): F[A] =
          E.handleWith(fa.validate(sender))(e => f(e).validate(sender))
      }

      override def pure[A](x: A): UserValidator[A] = new UserValidator[A] {
        override def validate[F[_]: Monad: ParserError](sender: RootSender): F[A] = x.pure
      }
    }
  }
}
