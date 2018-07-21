package net.katsstuff.scammander

import scala.annotation.implicitNotFound
import scala.language.higherKinds

import cats.Functor
import net.katsstuff.scammander.ScammanderHelper.ParserF

/**
  * A parameter for a command. Can convert a list of arguments into a given type.
  * @tparam A The parsed value.
  */
@implicitNotFound("Could not find a parameter for ${A}. Have you tried using OnlyOne")
trait ComplexParameter[F[_], A, RootSender, RunExtra, TabExtra] {

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
  def parse(source: RootSender, extra: RunExtra): ScammanderHelper.ParserF[F, A]

  /**
    * Returns the suggestions for a parameter.
    * @param source The command source.
    * @param extra Extra platform specific info about the command.
    * @return A list of the remaining arguments, and the suggestions.
    */
  def suggestions(source: RootSender, extra: TabExtra): ScammanderHelper.ParserF[F, Seq[String]]

  /**
    * The usage for this command.
    */
  def usage(source: RootSender): F[String]
}
object ComplexParameter {
  def apply[F[_], A, RootSender, RunExtra, TabExtra](
      implicit param: ComplexParameter[F, A, RootSender, RunExtra, TabExtra]
  ): ComplexParameter[F, A, RootSender, RunExtra, TabExtra] = param

  implicit def paramInstance[F[_], RootSender, RunExtra, TabExtra](
      implicit F: Functor[F]
  ): Functor[ComplexParameter[F, ?, RootSender, RunExtra, TabExtra]] =
    new Functor[ComplexParameter[F, ?, RootSender, RunExtra, TabExtra]] {

      override def map[A, B](fa: ComplexParameter[F, A, RootSender, RunExtra, TabExtra])(
          f: A => B
      ): ComplexParameter[F, B, RootSender, RunExtra, TabExtra] =
        new ComplexParameter[F, B, RootSender, RunExtra, TabExtra] {

          override def name: String = fa.name

          override def parse(source: RootSender, extra: RunExtra): ParserF[F, B] = fa.parse(source, extra).map(f)

          override def suggestions(source: RootSender, extra: TabExtra): ParserF[F, Seq[String]] =
            fa.suggestions(source, extra)

          override def usage(source: RootSender): F[String] = fa.usage(source)
        }
    }
}
