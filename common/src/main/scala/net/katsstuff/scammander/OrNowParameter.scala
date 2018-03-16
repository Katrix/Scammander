package net.katsstuff.scammander

import java.time.LocalDateTime

import net.katsstuff.scammander.CrossCompatibility._

trait OrNowParameter[RootSender, RunExtra, TabExtra] {
  self: ScammanderBase[RootSender, RunExtra, TabExtra]
    with OrParameters[RootSender, RunExtra, TabExtra]
    with NormalParameters[RootSender, RunExtra, TabExtra] =>

  /**
    * Given some parsed time, alternatively returns now instead.
    */
  sealed trait Now
  type OrNow[Base] = Base Or Now
  implicit val dateTimeOrNowParam: Parameter[LocalDateTime Or Now] = new Parameter[LocalDateTime Or Now] {
    override def name: String = dateTimeParam.name
    override def parse(
        source: RootSender,
        extra: RunExtra,
        xs: List[RawCmdArg]
    ): CommandStep[(List[RawCmdArg], LocalDateTime Or Now)] = {
      val (ys, res) = dateTimeParam.parse(source, extra, xs).getOrElse((xs, LocalDateTime.now))
      Right((ys, Or(res)))
    }

    override def suggestions(
        source: RootSender,
        extra: TabExtra,
        xs: List[RawCmdArg]
    ): Either[List[RawCmdArg], Seq[String]] =
      dateTimeParam.suggestions(source, extra, xs)

    override def usage(source: RootSender): String = s"[$name]"
  }
}
