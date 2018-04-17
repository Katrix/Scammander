package net.katsstuff.scammander

import java.time.LocalDateTime

import scala.language.higherKinds

import cats.data.StateT
import cats.syntax.all._

trait OrNowParameter[F[_], RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[F, RootSender, RunExtra, TabExtra]
    with OrParameters[F, RootSender, RunExtra, TabExtra]
    with NormalParameters[F, RootSender, RunExtra, TabExtra] =>

  /**
    * Given some parsed time, alternatively returns now instead.
    */
  sealed trait Now
  type OrNow[Base] = Base Or Now
  implicit val dateTimeOrNowParam: Parameter[LocalDateTime Or Now] = new Parameter[LocalDateTime Or Now] {
    override def name: String = dateTimeParam.name
    override def parse(source: RootSender, extra: RunExtra): StateT[F, List[RawCmdArg], LocalDateTime Or Now] =
      ScammanderHelper
        .withFallbackState(dateTimeParam.parse(source, extra), SF.pure(LocalDateTime.now()))
        .map(Or.apply)

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Seq[String]] =
      dateTimeParam.suggestions(source, extra)

    override def usage(source: RootSender): F[String] = s"[$name]".pure
  }
}
