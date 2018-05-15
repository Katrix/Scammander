package net.katsstuff.scammander

import org.scalatest.{Assertion, FunSuite, Matchers}

import cats.MonadError
import cats.data.NonEmptyList

class ScammanderSpec
    extends FunSuite
    with Matchers
    with ScammanderBaseAll[Either[NonEmptyList[CommandFailure], ?], Unit, Unit, Unit] {

  override type Result                   = Unit
  override type StaticChildCommand[Sender, Param] = DummyStaticChildCommand[Sender, Param]
  case class DummyStaticChildCommand[Sender, Param](command: Command[Sender, Param])
      extends BaseStaticChildCommand[Sender, Param]

  override protected val defaultCommandSuccess: Unit = ()

  override protected def tabExtraToRunExtra(extra: Unit): Unit = ()

  implicit override def F: MonadError[Either[CommandFailureNEL, ?], CommandFailureNEL] =
    cats.instances.either.catsStdInstancesForEither

  case class MyObj(name: String, i: Int)

  val myObjName1             = "foo"
  val myObjName2             = "bar"
  val myObjName3             = "baz"
  val myObjects: List[MyObj] = List(MyObj(myObjName1, 1), MyObj(myObjName2, 2), MyObj(myObjName3, 3))

  implicit val myObjHasName: HasName[MyObj]      = (a: MyObj) => a.name
  implicit val myObjparam: Parameter[Set[MyObj]] = Parameter.mkNamed("myobj", myObjects)

  def mkArgs(arguments: String): List[RawCmdArg] = ScammanderHelper.stringToRawArgsQuoted(arguments)

  def parse[A](arguments: String, checkEmpty: Boolean = true)(implicit param: Parameter[A]): Option[A] = {
    val parsed = param.parse((), ()).run(mkArgs(arguments))
    if (checkEmpty && arguments.nonEmpty) {
      parsed.foreach {
        case (xs, _) =>
          assert(xs.isEmpty)
      }
    }

    parsed.map(_._2).toOption
  }

  def error[A](arguments: String)(implicit param: Parameter[A]): CommandFailureNEL = {
    val parsed = param.parse((), ()).run(mkArgs(arguments))
    assert(parsed.isLeft)
    parsed.left.get
  }

  def singleError[A](arguments: String)(implicit param: Parameter[A]): CommandFailure = {
    val nel = error[A](arguments)
    assert(nel.size == 1)
    nel.head
  }

  def usage[A](implicit param: Parameter[A]): String = {
    val e = param.usage(())
    assert(e.isRight)
    e.right.get
  }

  def suggestions[A](arguments: String)(implicit param: Parameter[A]): Seq[String] = {
    val either = param.suggestions((), ()).run(mkArgs(arguments))
    assert(either.isRight)
    either.right.get._2
  }

  def noSuggestions[A](arguments: String)(implicit param: Parameter[A]): Assertion = {
    val either = param.suggestions((), ()).run(mkArgs(arguments)).map(_._2)
    assert(either.contains(Nil))
  }

  def errorSuggestions[A](arguments: String)(implicit param: Parameter[A]): Assertion = {
    val either = param.suggestions((), ()).run(mkArgs(arguments)).map(_._2)
    assert(either.isLeft)
  }
}
