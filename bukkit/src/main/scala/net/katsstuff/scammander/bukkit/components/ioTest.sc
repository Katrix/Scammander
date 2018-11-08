import scala.concurrent.duration.Duration

import cats.effect.IO

val fa = IO(5).map(_ + 5).flatMap(i => IO(i - 5))
fa.unsafeRunTimed(Duration.Zero)