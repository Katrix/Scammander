package net.katsstuff.scammander

import scala.annotation.implicitNotFound
import scala.language.higherKinds

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
