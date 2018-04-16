package net.katsstuff.scammander

import java.time.LocalDateTime

import scala.language.higherKinds

import cats.data.StateT

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
      for {
        xs <- ScammanderHelper.getArgs[F]
        res <- {
          val fa1 = dateTimeParam.parse(source, extra).run(xs)

          val res = F.handleError(fa1)(_ => (xs, LocalDateTime.now()))

          Command.liftFStateParse(res).transform((_, t) => t)
        }
      } yield Or(res)

    override def suggestions(source: RootSender, extra: TabExtra): StateT[F, List[RawCmdArg], Option[Seq[String]]] =
      dateTimeParam.suggestions(source, extra)

    override def usage(source: RootSender): F[String] = F.pure(s"[$name]")
  }
}
