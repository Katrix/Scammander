package net.katsstuff.scammander

import scala.language.higherKinds

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._

class MiscHelperParamSpec extends ScammanderSpec {

  implicit val sourceAsDouble: UserValidator[Double] = new UserValidator[Double] {
    override def validate[F[_]: Monad: ParserError](sender: Unit): F[Double] = 0D.pure
  }
  implicit val sourceAsMyObj: UserValidator[MyObj] = new UserValidator[MyObj] {
    override def validate[F[_]: Monad: ParserError](sender: Unit): F[MyObj] = MyObj(myObjName2, 2).pure
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

  test("An or source parameter should still retain the original suggestions") {
    suggestions[OnlyOne[MyObj] Or Source]("") should equal(suggestions[OnlyOne[MyObj]](""))
    suggestions[OnlyOne[MyObj] Or Source]("f") should equal(suggestions[OnlyOne[MyObj]]("f"))
    suggestions[OnlyOne[MyObj] Or Source]("ba") should equal(suggestions[OnlyOne[MyObj]]("ba"))
  }

  test("One or more should parse as many as possible") {
    parse[OneOrMore[Int]]("5 4 3") should contain(OneOrMore(NonEmptyList.of(5, 4, 3)))
    parse[OneOrMore[Int]]("1 2") should contain(OneOrMore(NonEmptyList.of(1, 2)))
    parse[OneOrMore[Int]]("1") should contain(OneOrMore(NonEmptyList.of(1)))
    parse[OneOrMore[Int]]("") should equal(None)
  }

  test("Zero or more should parse as many as possible") {
    parse[ZeroOrMore[Int]]("5 4 3") should contain(ZeroOrMore(Seq(5, 4, 3)))
    parse[ZeroOrMore[Int]]("1 2") should contain(ZeroOrMore(Seq(1, 2)))
    parse[ZeroOrMore[Int]]("1") should contain(ZeroOrMore(Seq(1)))
    parse[ZeroOrMore[Int]]("") should contain(ZeroOrMore(Nil))
  }
}
