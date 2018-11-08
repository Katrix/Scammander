package net.katsstuff.scammander

import scala.language.higherKinds

import scala.annotation.implicitNotFound

import cats.syntax.all._
import cats.{Functor, Monad}
import net.katsstuff.scammander.ScammanderTypes._

/**
  * A parameter for a command. Can convert a list of arguments into a given type.
  * @tparam A The parsed value.
  */
@implicitNotFound("Could not find a parameter for ${A}. Have you tried using OnlyOne")
trait ComplexParameter[A, RootSender, RunExtra, TabExtra] {

  /**
    * The name given to the parameter to show to users-
    */
  def name: String

  /**
    * Parse a list of arguments into the type of this parameter.
    * @param source The command source.
    * @param extra Extra platform specific info about the command.
    * @return A command step with the remaining arguments, and the parsed type.
    */
  def parse[F[_]: Monad: ParserState: ParserError](source: RootSender, extra: RunExtra): F[A]

  /**
    * Returns the suggestions for a parameter.
    * @param source The command source.
    * @param extra Extra platform specific info about the command.
    * @return A list of the remaining arguments, and the suggestions.
    */
  def suggestions[F[_]: Monad: ParserState: ParserError](source: RootSender, extra: TabExtra): F[Seq[String]]

  /**
    * The usage for this command.
    */
  def usage[F[_]: Monad: ParserError](source: RootSender): F[String]
}
object ComplexParameter {
  def apply[A, RootSender, RunExtra, TabExtra](
      implicit param: ComplexParameter[A, RootSender, RunExtra, TabExtra]
  ): ComplexParameter[A, RootSender, RunExtra, TabExtra] = param

  implicit def paramInstance[RootSender, RunExtra, TabExtra]
    : Functor[ComplexParameter[?, RootSender, RunExtra, TabExtra]] =
    new Functor[ComplexParameter[?, RootSender, RunExtra, TabExtra]] {

      override def map[A, B](fa: ComplexParameter[A, RootSender, RunExtra, TabExtra])(
          f: A => B
      ): ComplexParameter[B, RootSender, RunExtra, TabExtra] =
        new ComplexParameter[B, RootSender, RunExtra, TabExtra] {

          override def name: String = fa.name

          override def parse[F[_]: Monad: ParserState: ParserError](source: RootSender, extra: RunExtra): F[B] =
            fa.parse(source, extra).map(f)

          override def suggestions[F[_]: Monad: ParserState: ParserError](
              source: RootSender,
              extra: TabExtra
          ): F[Seq[String]] =
            fa.suggestions(source, extra)

          override def usage[F[_]: Monad: ParserError](source: RootSender): F[String] = fa.usage(source)
        }
    }
}
