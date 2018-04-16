package net.katsstuff.scammander

class MkNamedSpec extends ScammanderSpec {

  test("MkNamed should parse valid values") {
    def testArgs(args: String, obj: MyObj) =
      parse[OnlyOne[MyObj]](args) should contain(OnlyOne(obj))

    testArgs(myObjName1, MyObj(myObjName1, 1))
    testArgs(myObjName2, MyObj(myObjName2, 2))
    testArgs(myObjName3, MyObj(myObjName3, 3))
  }

  test("MkNamed usage should makes sense") {
    usage[OnlyOne[MyObj]] should equal("<myobj>")
  }

  test("MkNamed suggestions should complete correctly") {
    def testArgs(args: String, testAgainst: String) =
      suggestions[OnlyOne[MyObj]](args).mkString(" ") should equal(testAgainst)

    testArgs("", s"$myObjName1 $myObjName2 $myObjName3")
    testArgs("f", s"$myObjName1")
    testArgs("ba", s"$myObjName2 $myObjName3")
    noSuggestions[OnlyOne[MyObj]]("bar")
  }

  test("MkNamed should have a good error message") {
    singleError[OnlyOne[MyObj]]("") should equal(Command.syntaxError("Not enough arguments", -1))
    singleError[OnlyOne[MyObj]]("bin") should equal(Command.usageError("bin is not a valid myobj", 0))
  }
}
