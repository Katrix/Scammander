package net.katsstuff.scammander

import shapeless.{Witness => W}
import shapeless._

class FlagsParamSpec extends ScammanderSpec {

  case class MyArg3(foo: Int, bar: String, baz: Double)
  implicit val myParam3: Parameter[MyArg3] = ParameterDeriver[MyArg3].derive

  test("Value flags should parse values wherever they are") {
    def testArgs(args: String, checkEmpty: Boolean = true) =
      parse[ValueFlag[W.`"c"`.T, Int]](args, checkEmpty) should contain(ValueFlag(Some(5)))

    testArgs("-c 5")
    testArgs("foo -c 5 bar", checkEmpty = false)
  }

  test("Value flags usage makes sense") {
    def testUsage[Param: Parameter](testAgainst: String) = usage[Param] should equal(testAgainst)

    testUsage[ValueFlag[W.`"c"`.T, Int]]("-c <int>")
    testUsage[ValueFlag[W.`"long"`.T, Int]]("--long <int>")
  }

  test("Value flags suggestions should complete correctly") {
    def testArgs(args: String, testAgainst: String) =
      suggestions[ValueFlag[W.`"long"`.T, OnlyOne[MyObj]]](args).mkString(" ") should equal(testAgainst)

    testArgs("-", "--long")
    testArgs("--long ", s"$myObjName1 $myObjName2 $myObjName3")
    testArgs("--long f", s"$myObjName1")
    testArgs("--long ba", s"$myObjName2 $myObjName3")
    noSuggestions[ValueFlag[W.`"long"`.T, OnlyOne[MyObj]]]("")
    noSuggestions[ValueFlag[W.`"long"`.T, OnlyOne[MyObj]]]("--long bar")
  }

  test("Boolean flags should parse flags") {
    def testArgs(args: String, checkEmpty: Boolean = true) =
      parse[BooleanFlag[W.`"c"`.T]](args, checkEmpty) should contain(BooleanFlag(true))

    testArgs("-c")
    testArgs("foo -c 5 bar", checkEmpty = false)
  }

  test("Boolean flags usage makes sense") {
    def testUsage[Param: Parameter](testAgainst: String) = usage[Param] should equal(testAgainst)

    testUsage[BooleanFlag[W.`"c"`.T]]("-c")
    testUsage[BooleanFlag[W.`"long"`.T]]("--long")
  }

  test("Boolean flags suggestions should complete correctly") {
    def testArgs(args: String, testAgainst: String) =
      suggestions[BooleanFlag[W.`"long"`.T]](args).mkString(" ") should equal(testAgainst)

    testArgs("-", "--long")
    testArgs("--l", "--long")
    noSuggestions[BooleanFlag[W.`"long"`.T]]("")
    noSuggestions[BooleanFlag[W.`"long"`.T]]("--long")
  }

  test("Flags should be parsed wherever, without interrupting normal parsing") {
    val res                    = Flags(ValueFlag(Some(3)), MyArg3(5, "foo", 2.3))
    def testArgs(args: String) = parse[Flags[ValueFlag[W.`"c"`.T, Int], MyArg3]](args) should contain(res)

    testArgs("5 foo 2.3 -c 3")
    testArgs("5 foo -c 3 2.3")
    testArgs("5 -c 3 foo 2.3")
    testArgs("-c 3 5 foo 2.3")
  }

  test("Multiple flags should work using HLists") {
    val res = Flags(ValueFlag(Some(3)) :: BooleanFlag(true) :: HNil, MyArg3(5, "foo", 2.3))
    def testArgs(args: String) =
      parse[Flags[ValueFlag[W.`"c"`.T, Int] :: BooleanFlag[W.`"d"`.T] :: HNil, MyArg3]](args) should contain(res)

    testArgs("5 foo -c 3 2.3 -d")
    testArgs("5 foo 2.3 -c 3 -d")
    testArgs("5 foo 2.3 -d -c 3")
    testArgs("5 foo -d 2.3 -c 3")
  }
}
