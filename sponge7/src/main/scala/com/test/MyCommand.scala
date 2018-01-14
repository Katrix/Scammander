package com.test

import org.spongepowered.api.entity.living.player.Player
import org.spongepowered.api.text.Text

import net.katsstuff.scammander.sponge._

object MyCommand {

  Command.simple { (src, _, msg: String) =>
    src.sendMessage(Text.of(s"ECHO: $msg"))
    Command.success()
  }.register(
    ???,
    Alias("echo"),
    Permission("foo.bar.echo"),
    Description(Text.of("ECHO echo echo..."))
  )

  case class MyArg(count: Int, player: Player Or Source, remaining: RemainingAsString)
  Command.withSender { (src: Player, _, arg: MyArg) =>
    val MyArg(count, Or(player), RemainingAsString(msg)) = arg
    for(_ <- 0 until count) {
      player.sendMessage(Text.of(msg))
    }

    src.sendMessage(Text.of(s"Sent spam to ${player.getName}"))

    Command.success()
  }.register(
    ???,
    Alias("Spam"),
    Permission("foo.bar.spam"),
    Description(Text.of("Spam your enemies into oblivion"))
  )
}
