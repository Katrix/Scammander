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
    error[Byte]("foo").msg should equal("foo is not a valid byte")
    error[Short]("foo").msg should equal("foo is not a valid short")
    error[Int]("foo").msg should equal("foo is not a valid int")
    error[Long]("foo").msg should equal("foo is not a valid long")
    error[Float]("foo").msg should equal("foo is not a valid float")
    error[Double]("foo").msg should equal("foo is not a valid double")
    error[BigInt]("foo").msg should equal("foo is not a valid integer number")
    error[BigDecimal]("foo").msg should equal("foo is not a valid decimal number")
    error[UUID]("foo").msg should equal("foo is not a valid uuid")
  }
}
