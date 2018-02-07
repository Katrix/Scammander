package net.katsstuff.scammander

class MiscHelperParamSpec extends ScammanderSpec {

  implicit val sourceAsDouble: UserValidator[Double] = UserValidator.mkValidator(_ => Right(0D))
  implicit val sourceAsMyObj: UserValidator[MyObj] = UserValidator.mkValidator(_ => Right(MyObj(myObjName2, 2)))

  test("An or source parameter should work when passed parameters") {
    parse[Double Or Source]("2.3") should contain(Or(2.3D))
  }

  test("An or source parameter should be optional") {
    parse[Double Or Source]("") should contain(Or(0D))
  }

  test("An or source parameter should be have optional usage") {
    assert(usage[Double Or Source] == "[double]")
  }

  test("An or source parameter should still retain the original suggestions") {
    suggestions[OnlyOne[MyObj] Or Source]("") should equal(suggestions[OnlyOne[MyObj]](""))
    suggestions[OnlyOne[MyObj] Or Source]("f") should equal(suggestions[OnlyOne[MyObj]]("f"))
    suggestions[OnlyOne[MyObj] Or Source]("ba") should equal(suggestions[OnlyOne[MyObj]]("ba"))
  }

  test("All of should parse as many as possible") {
    parse[AllOff[Int]]("5 4 3") should contain(AllOff(Seq(5, 4, 3)))
    parse[AllOff[Int]]("1") should contain(AllOff(Seq(1)))
    parse[AllOff[Int]]("") should contain(AllOff(Nil))
  }
}
