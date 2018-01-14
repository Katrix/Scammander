package net.katsstuff.scammander

import org.scalatest.{FunSuite, Matchers}

import net.katsstuff.scammander.misc.RawCmdArg

import shapeless.{Witness => W}
import shapeless._

class ParameterSpec extends FunSuite with Matchers with ScammanderUniverse[Unit, Unit, Unit] {

  def mkArgs(arguments: String): List[RawCmdArg] = ScammanderHelper.stringToRawArgs(arguments)

  def parse[A](arguments: String)(implicit param: Parameter[A]): Option[A] =
    param.parse((), (), mkArgs(arguments)).map(_._2).toOption

  def usage[A](implicit param: Parameter[A]): String = param.usage(())

  def suggestions[A](arguments: String)(implicit param: Parameter[A]): Seq[String] =
    param.suggestions((), (), mkArgs(arguments))._2

  implicit val sourceAsDouble: UserValidator[Double] = UserValidator.mkValidator(_ => Right(0D))

  test("A primitive parameter should parse what's asked for") {
    parse[Int]("5") should contain(5)
  }

  test("A primitive parameter should have a simple usage") {
    assert(usage[Int] == "<int>")
  }

  case class MyArg2(foo: Int, bar: String)
  test("A product(2) parameter should parse both parameters") {
    parse[MyArg2]("5 foo") should contain(MyArg2(5, "foo"))
  }

  test("A product(2) parameter should contain usage for both with correct names") {
    assert(usage[MyArg2] == "<foo> <bar>")
  }

  case class MyArg3(foo: Int, bar: String, baz: Double)
  test("A product(3) parameter should parse all parameters") {
    parse[MyArg3]("5 foo 2.3") should contain(MyArg3(5, "foo", 2.3D))
  }

  test("A product(3) parameter should contain usage for all with correct names") {
    assert(usage[MyArg3] == "<foo> <bar> <baz>")
  }

  sealed trait MySumArg2
  object MySumArg2 {
    case class Arg1(foo: Int, bar: String)    extends MySumArg2
    case class Arg2(foo: String, bar: String) extends MySumArg2
  }

  test("A sum(2) parameter should parse all parameters Arg1") {
    parse[MySumArg2]("5 foo") should contain(MySumArg2.Arg1(5, "foo"))
    parse[MySumArg2]("foo bar") should contain(MySumArg2.Arg2("foo", "bar"))
  }

  test("A sum(2) parameter should contain usage for all with types correct names") {
    assert(usage[MySumArg2] == "(<foo> <bar>)|(<foo> <bar>)")
  }

  sealed trait MySumArg3
  object MySumArg3 {
    case class Arg1(foo: Int)                           extends MySumArg3
    case class Arg2(bar: String, foo: Int)              extends MySumArg3
    case class Arg3(baz: Double, bar: String, foo: Int) extends MySumArg3
  }

  test("A sum(3) parameter should parse all parameters") {
    parse[MySumArg3]("5") should contain(MySumArg3.Arg1(5))
    parse[MySumArg3]("bar 5") should contain(MySumArg3.Arg2("bar", 5))
    parse[MySumArg3]("2.3 bar 5") should contain(MySumArg3.Arg3(2.3D, "bar", 5))
  }

  test("A sum(3) parameter should contain usage for all with types correct names") {
    assert(usage[MySumArg3] == "(<foo>)|(<bar> <foo>)|(<baz> <bar> <foo>)")
  }

  test("An or source parameter should work when passed parameters") {
    parse[Double Or Source]("2.3") should contain(Or(2.3D))
  }

  test("An or source parameter should be optional") {
    parse[Double Or Source]("") should contain(Or(0D))
  }

  test("An or source parameter should be have optional usage") {
    assert(usage[Double Or Source] == "[double]")
  }

  test("All of should parse as many as possible") {
    parse[AllOff[Int]]("5 4 3") should contain(AllOff(Seq(5, 4, 3)))
    parse[AllOff[Int]]("1") should contain(AllOff(Seq(1)))
    parse[AllOff[Int]]("") should contain(AllOff(Nil))
  }

  test("Value flags should parse values wherever they are") {
    parse[ValueFlag[W.`"c"`.T, Int]]("-c 5") should contain(ValueFlag(Some(5)))
    parse[ValueFlag[W.`"c"`.T, Int]]("foo -c 5 bar") should contain(ValueFlag(Some(5)))
  }

  test("Boolean flags should parse flags") {
    parse[BooleanFlag[W.`"c"`.T]]("-c") should contain(BooleanFlag(true))
    parse[BooleanFlag[W.`"c"`.T]]("foo -c 5 bar") should contain(BooleanFlag(true))
  }

  test("Flags should be parsed wherever, without interupting normal pasing") {
    val res = Flags(ValueFlag(Some(3)), MyArg3(5, "foo", 2.3))

    parse[Flags[ValueFlag[W.`"c"`.T, Int], MyArg3]]("5 foo 2.3 -c 3") should contain(res)
    parse[Flags[ValueFlag[W.`"c"`.T, Int], MyArg3]]("5 foo -c 3 2.3") should contain(res)
    parse[Flags[ValueFlag[W.`"c"`.T, Int], MyArg3]]("5 -c 3 foo 2.3") should contain(res)
    parse[Flags[ValueFlag[W.`"c"`.T, Int], MyArg3]]("-c 3 5 foo 2.3") should contain(res)
  }

  test("Multiple flags should work using HLists") {
    val res = Flags(ValueFlag(Some(3)) :: BooleanFlag(true) :: HNil, MyArg3(5, "foo", 2.3))
    parse[Flags[ValueFlag[W.`"c"`.T, Int] :: BooleanFlag[W.`"d"`.T] :: HNil, MyArg3]]("5 foo -c 3 2.3 -d") should contain(
      res
    )
  }
}
