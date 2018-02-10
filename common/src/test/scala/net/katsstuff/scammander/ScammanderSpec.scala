package net.katsstuff.scammander

import org.scalatest.{Assertion, FunSuite, Matchers}

import net.katsstuff.scammander.misc.{HasName, RawCmdArg}

class ScammanderSpec extends FunSuite with Matchers with ScammanderUniverse[Unit, Unit, Unit] {

  override protected type Result             = Unit
  override protected type StaticChildCommand = Unit
  override protected val defaultCommandSuccess: Unit = ()

  override protected def tabExtraToRunExtra(extra: Unit): Unit = ()

  case class MyObj(name: String, i: Int)

  val myObjName1 = "foo"
  val myObjName2 = "bar"
  val myObjName3 = "baz"
  val myObjects: List[MyObj] = List(MyObj(myObjName1, 1), MyObj(myObjName2, 2), MyObj(myObjName3, 3))

  implicit val myObjHasName: HasName[MyObj]        = (a: MyObj) => a.name
  implicit val myObjparam:   Parameter[Set[MyObj]] = Parameter.mkNamed("myobj", myObjects)

  def mkArgs(arguments: String): List[RawCmdArg] = ScammanderHelper.stringToRawArgsQuoted(arguments)

  def parse[A](arguments: String, checkEmpty: Boolean = true)(implicit param: Parameter[A]): Option[A] = {
    val parsed = param.parse((), (), mkArgs(arguments))
    if (checkEmpty && arguments.nonEmpty) {
      parsed.foreach {
        case (xs, _) =>
          assert(xs.isEmpty)
      }
    }

    parsed.map(_._2).toOption
  }

  def error[A](arguments: String)(implicit param: Parameter[A]): CommandFailure = {
    val parsed = param.parse((), (), mkArgs(arguments))
    assert(parsed.isLeft)
    parsed.left.get
  }

  def usage[A](implicit param: Parameter[A]): String = param.usage(())

  def suggestions[A](arguments: String)(implicit param: Parameter[A]): Seq[String] = {
    val either = param.suggestions((), (), mkArgs(arguments))
    assert(either.isRight)
    either.right.get
  }

  def noSuggestions[A](arguments: String)(implicit param: Parameter[A]): Assertion = {
    val either = param.suggestions((), (), mkArgs(arguments))
    assert(either.isLeft || either.contains(Nil))
  }

}
