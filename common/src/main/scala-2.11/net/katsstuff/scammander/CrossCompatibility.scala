package net.katsstuff.scammander

import scala.util.{Either, Failure, Success, Try}

//Either is right associative here
object CrossCompatibility {

  implicit class RichEither[A, B](private val either: Either[A, B]) extends AnyVal {
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = either.right.flatMap(f)
    def map[B1](f: B => B1):                          Either[A, B1]  = either.right.map(f)
    def getOrElse[B1 >: B](or: => B1):                B1             = either.right.getOrElse(or)
  }

  implicit class RichTry[A](private val tryObj: Try[A]) extends AnyVal {
    def toEither: Either[Throwable, A] = tryObj match {
      case Success(a) => Right(a)
      case Failure(e) => Left(e)
    }
  }
}
