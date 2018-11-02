package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Functor
import cats.syntax.all._
import net.katsstuff.scammander.ScammanderTypes.ParserError

/**
  * A typeclass which helps convert a user into another type.
  */
trait ComplexUserValidator[A, RootSender] {

  /**
    * Validates the sender.
    */
  def validate[F[_]: ParserError](sender: RootSender): F[A]
}
object ComplexUserValidator {
  def apply[A, RootSender](
      implicit validator: ComplexUserValidator[A, RootSender]
  ): ComplexUserValidator[A, RootSender] = validator

  implicit def instanceForValidator[RootSender]: Functor[ComplexUserValidator[?, RootSender]] =
    new Functor[ComplexUserValidator[?, RootSender]] {
      //noinspection ConvertExpressionToSAM
      override def map[A, B](fa: ComplexUserValidator[A, RootSender])(
          f: A => B
      ): ComplexUserValidator[B, RootSender] = new ComplexUserValidator[B, RootSender] {
        override def validate[F[_]: ParserError](sender: RootSender): F[B] = fa.validate(sender).map(f)
      }
    }
}
