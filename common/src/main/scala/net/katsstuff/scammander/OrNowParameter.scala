package net.katsstuff.scammander

import scala.language.higherKinds

import java.time.LocalDateTime

import cats.Monad
import cats.effect.Async
import cats.syntax.all._

trait OrNowParameter {
  self: ScammanderBase with OrParameters with NormalParameters =>

  /**
    * Given some parsed time, alternatively returns now instead.
    */
  sealed trait Now
  type OrNow[Base] = Base Or Now
  implicit val dateTimeOrNowParam: Parameter[LocalDateTime Or Now] = new Parameter[LocalDateTime Or Now] {

    override val name: String = dateTimeParam.name

    override def parse[F[_]: Monad: ParserState: ParserError](
        source: RootSender,
        extra: RunExtra
    ): F[LocalDateTime Or Now] =
      ScammanderHelper
        .withFallback(dateTimeParam.parse(source, extra), LocalDateTime.now().pure)
        .map(Or.apply)

    override def suggestions[F[_]: Async: ParserState: ParserError](
        source: RootSender,
        extra: TabExtra
    ): F[Seq[String]] =
      dateTimeParam.suggestions(source, extra)

    override def usage[F[_]: Monad: ParserError](source: RootSender): F[String] = s"[$name]".pure
  }
}
