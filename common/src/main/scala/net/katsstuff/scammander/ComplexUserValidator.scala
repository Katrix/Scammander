package net.katsstuff.scammander

import scala.language.higherKinds

/**
  * A typeclass which helps convert a user into another type.
  */
trait ComplexUserValidator[F[_], A, RootSender] {

  /**
    * Validates the sender.
    */
  def validate(sender: RootSender): F[A]
}
