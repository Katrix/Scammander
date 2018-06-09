package net.katsstuff.scammander.sponge.components

import scala.language.higherKinds

import net.katsstuff.scammander.ScammanderBaseAll

trait SpongeBaseAll[F[_]]
    extends SpongeBase[F]
    with ScammanderBaseAll[F]
    with SpongeValidators[F]
    with SpongeParameter[F]
    with SpongeOrParameter[F]
    with SpongeHelpCommands[F]
