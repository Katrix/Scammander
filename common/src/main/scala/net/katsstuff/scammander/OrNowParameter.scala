package net.katsstuff.scammander

import java.time.LocalDateTime

import scala.language.higherKinds

import cats.syntax.all._

trait OrNowParameter[F[_]] {
  self: ScammanderBase[F]
    with OrParameters[F]
    with NormalParameters[F] =>

  /**
    * Given some parsed time, alternatively returns now instead.
    */
  sealed trait Now
  type OrNow[Base] = Base Or Now
  implicit val dateTimeOrNowParam: Parameter[LocalDateTime Or Now] = new Parameter[LocalDateTime Or Now] {

    override val name: String = dateTimeParam.name

    override def parse(source: RootSender, extra: RunExtra): Parser[LocalDateTime Or Now] =
      ScammanderHelper
        .withFallbackParser(dateTimeParam.parse(source, extra), parser.pure(LocalDateTime.now()))
        .map(Or.apply)

    override def suggestions(source: RootSender, extra: TabExtra): Parser[Seq[String]] =
      dateTimeParam.suggestions(source, extra)

    override def usage(source: RootSender): F[String] = s"[$name]".pure
  }
}
