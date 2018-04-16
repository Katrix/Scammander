package net.katsstuff.scammander

import java.util.UUID

class PrimitiveParamSpec extends ScammanderSpec {

  test("A primitive parameter should parse what's asked for") {
    parse[Int]("5") should contain(5)
  }

  test("A primitive parameter should have a simple usage") {
    assert(usage[Int] == "<int>")
  }

  test("Primitive parameters should give no suggestions") {
    noSuggestions[Int]("")
    noSuggestions[Int]("5")
  }

  test("UUID should be parsed successfully") {
    val uuid = UUID.randomUUID()
    parse[UUID](uuid.toString) should contain(uuid)
  }

  test("Number parameters should have a good error message") {
    singleError[Byte]("foo").msg should equal("foo is not a valid byte")
    singleError[Short]("foo").msg should equal("foo is not a valid short")
    singleError[Int]("foo").msg should equal("foo is not a valid int")
    singleError[Long]("foo").msg should equal("foo is not a valid long")
    singleError[Float]("foo").msg should equal("foo is not a valid float")
    singleError[Double]("foo").msg should equal("foo is not a valid double")
    singleError[BigInt]("foo").msg should equal("foo is not a valid integer number")
    singleError[BigDecimal]("foo").msg should equal("foo is not a valid decimal number")
    singleError[UUID]("foo").msg should equal("foo is not a valid uuid")
  }
}
