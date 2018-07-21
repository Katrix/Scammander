package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Functor
import cats.syntax.all._

/**
  * A typeclass which helps convert a user into another type.
  */
trait ComplexUserValidator[F[_], A, RootSender] {

  /**
    * Validates the sender.
    */
  def validate(sender: RootSender): F[A]
}
object ComplexUserValidator {
  def apply[F[_], A, RootSender](
      implicit validator: ComplexUserValidator[F, A, RootSender]
  ): ComplexUserValidator[F, A, RootSender] = validator

  implicit def instanceForValidator[F[_], RootSender](
      implicit F: Functor[F]
  ): Functor[ComplexUserValidator[F, ?, RootSender]] = new Functor[ComplexUserValidator[F, ?, RootSender]] {
    //noinspection ConvertExpressionToSAM
    override def map[A, B](fa: ComplexUserValidator[F, A, RootSender])(
        f: A => B
    ): ComplexUserValidator[F, B, RootSender] = new ComplexUserValidator[F, B, RootSender] {
      override def validate(sender: RootSender): F[B] = fa.validate(sender).map(f)
    }
  }
}
