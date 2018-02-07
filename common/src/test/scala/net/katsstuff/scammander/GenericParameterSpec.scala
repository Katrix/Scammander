package net.katsstuff.scammander

class GenericParameterSpec extends ScammanderSpec {

  case class MyArg2(foo: Int, bar: String)
  implicit val myParam2: Parameter[MyArg2] = Parameter.deriver[MyArg2].derive
  test("A product(2) parameter should parse both parameters") {
    parse[MyArg2]("5 foo") should contain(MyArg2(5, "foo"))
  }

  test("A product(2) parameter should contain usage for both with correct names") {
    assert(usage[MyArg2] == "<foo> <bar>")
  }

  case class SuggestionArg(myObj1: OnlyOne[MyObj], i: Int, myObj2: OnlyOne[MyObj])
  implicit val suggestionParam: Parameter[SuggestionArg] = Parameter.deriver[SuggestionArg].derive
  test("A product(3) parameter should return the correct suggestions") {
    def testArgs(args: String, testAgainst: String) =
      suggestions[SuggestionArg](args).mkString(" ") should equal(testAgainst)

    println(suggestions[SuggestionArg](""))

    testArgs("", s"$myObjName1 $myObjName2 $myObjName3")
    testArgs("f", s"$myObjName1")
    testArgs("ba", s"$myObjName2 $myObjName3")
    testArgs("bar", s"$myObjName1 $myObjName2 $myObjName3")
    testArgs("bar 5 f", s"$myObjName1")
    testArgs("bar 5 ba", s"$myObjName2 $myObjName3")
    testArgs("bar 5", s"$myObjName1 $myObjName2 $myObjName3")
    noSuggestions[SuggestionArg]("bar 5 foo")
  }

  case class MyArg3(foo: Int, bar: String, baz: Double)
  implicit val myParam3: Parameter[MyArg3] = Parameter.deriver[MyArg3].derive
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
    implicit val param1: Parameter[Arg1] = Parameter.deriver[Arg1].derive
    implicit val param2: Parameter[Arg2] = Parameter.deriver[Arg2].derive
  }

  implicit val mySumParam2: Parameter[MySumArg2] = Parameter.deriver[MySumArg2].derive
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
    implicit val param1: Parameter[Arg1] = Parameter.deriver[Arg1].derive
    implicit val param2: Parameter[Arg2] = Parameter.deriver[Arg2].derive
    implicit val param3: Parameter[Arg3] = Parameter.deriver[Arg3].derive
  }

  implicit val mySumParam3: Parameter[MySumArg3] = Parameter.deriver[MySumArg3].derive
  test("A sum(3) parameter should parse all parameters") {
    parse[MySumArg3]("5") should contain(MySumArg3.Arg1(5))
    parse[MySumArg3]("bar 5") should contain(MySumArg3.Arg2("bar", 5))
    parse[MySumArg3]("2.3 bar 5") should contain(MySumArg3.Arg3(2.3D, "bar", 5))
  }

  test("A sum(3) parameter should contain usage for all with types correct names") {
    assert(usage[MySumArg3] == "(<foo>)|(<bar> <foo>)|(<baz> <bar> <foo>)")
  }
}
