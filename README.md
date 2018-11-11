# Scammander
*An extensible, typesafe command library for Minecraft*

Scammander is a command library for Scala that focues on typesafe and simple commands for Minecraft. A command is just a name with a bunch of arguments. The arguments are then parsed, validated, and then used in the command. Often that process is tedious and boring, and takes away from the real meat of what a command is really doing. Scammander aims to remove that problem by reduce boilerplate and keeping things nice and easy.

Big thanks to the Sponge library for some of the ideas and designs that are used in Scammander. If you know how to use one, using the other will hopefully not be too hard.

## Basic Usage
While Scammander is still early in development, you can try Scammander by adding one of these to your `build.sbt`.
```scala
//Core library. Use this if you want to create an implmentation for a new platform
libraryDependencies += "net.katsstuff" %% "scammander"                  % "0.6"
libraryDependencies += "net.katsstuff" %% "scammander-sponge_sponge5.1" % "0.6" //Sponge API 5.1 platform
libraryDependencies += "net.katsstuff" %% "scammander-sponge_sponge6.0" % "0.6" //Sponge API 6 platform
libraryDependencies += "net.katsstuff" %% "scammander-sponge_sponge7.0" % "0.6" //Sponge API 7 platform
libraryDependencies += "net.katsstuff" %% "scammander-bukkit"           % "0.6" //Bukkit platform
```

So, how do you use Scammander. Let's go over how to use it with Sponge. Usage for the other platforms should be somewhat similar. All examples here assumes.
```scala
import net.katsstuff.scammander.sponge._
```

### Commands
First, defining a command. If your command isn't too complex, you can use the `Command.simple`, `Command.withSender`, `Command.withChildren` and `Command.withSenderAndChildren` methods. These are demonstrated below.

```scala
//Make sure to supply the type annotation for the parameter (the int)
Command
  .simple[Int] { (sender, _, int) =>
    sender.sendMessage(Text.of(s"Hi $int"))
    Result.successF()
  }
  .register(plugin = myPlugin, aliases = Seq("hi"))

//We can use case classes and sealed trait hierarchies as parameters
case class MyParam(count: Int, message: RemainingAsString)
object MyParam {
  implicit val param: Parameter[MyParam] = ParameterDeriver[MyParam].derive
}

//We use the withSender method if we want a custom sender. Make sure to supply
//the type for the sender too.
Command
  .withSender[Player, MyParam] { (sender, _, param) =>
    for (i <- 0 until param.count) sender.sendMessage(Text.of(s"$i ${param.message}"))
    Result.successF()
  }
  //Here we supply extra stuff when registering the command
  .register(
    plugin = myPlugin,
    aliases = Seq("spam"),
    permission = Some("foo.bar.baz"),
    //Help here is an object with an apply method to simplify creating help messages
    shortDescription = Help(Text.of("Spam yourself to death")),
  )
```

For more complex uses you can also extend Command yourself. For example if you want a custom usage or suggestions.

### Parameters
While parameters are derived for case classes and the like, you can also define your own by making sure there is an implicit of type `Parameter[MyType]` in scope.

## More complex uses
So, having gone over the basics, let's looks at some complex uses.

### UserValidator
Often times you find yourself limiting a command to only some types of users. You can remove that logic from the command itself by instead using the `UserValidator` typeclass. Instances for the most common uses are already defined.

### Extending base traits
Scammander is designed to not be tied down to one specific platform. You can use it with Sponge, Bukkit, Forge, or whatever other custom stuff you have. To do so, just extend ScammanderBase, fill in the types and methods missing and go from there. You can also extend one of the existing universe base if you want to add more implicits without cluttering up your imports.

## WIP
Scammander is still heavily a work in progress project. As such there are many things that are either not working currently, or working poorly. Under you can see some of those, and plans for them.
- [x] Sponge implementation
- [x] Bukkit implementation
- [ ] Bukkit implementation with internal/NMS
- [ ] Forge implementation
- [x] Child commands
- [x] Flags
- [ ] Better error messages
- [x] A way to handle X or else source
- [ ] More stuff I can't remember now

## Why another library?
Creating a library for Bukkit or Forge might be obvious, as doing commands there is already tedious, and while this library does support those platforms, it was initially made for Sponge in mind. So, the question is then why? There are two main answers to that question. 

The first one is about the shortcomings of the Sponge API. Don't get me wrong, I think the command API for Sponge is wonderful, but it also has some shortcomings that are hard to fix in Java without doing lots of mucky reflection. Many of the same shortcomings that can be easily fixed using typeclasses when working in Scala, and that's mostly what Scammander is about. 

While a simple command in both the Sponge API and Scammander are laughably simple, the amount of complexity needed increases much quicker as you add more parameters with Sponge. If you want to group your stuff in a class in Sponge, you have to write the class for it yourself, while in Scammander it's easily derived for you. 

The other reason? I wanted an awesome command API like the one in Sponge for other platforms.