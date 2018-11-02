package net.katsstuff.scammander.sponge.components

import net.katsstuff.scammander.ScammanderBaseAll

trait SpongeBaseAll
    extends SpongeBase
    with ScammanderBaseAll
    with SpongeValidators
    with SpongeParameter
    with SpongeOrParameter
    with SpongeHelpCommands
