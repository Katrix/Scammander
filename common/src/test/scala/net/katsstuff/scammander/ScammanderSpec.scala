package net.katsstuff.scammander

import cats.Monad
import cats.data.{EitherT, StateT}
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.instances.either._
import cats.mtl.instances.all._
import cats.mtl.{DefaultMonadState, MonadState}
import org.scalatest.{Assertion, FunSuite, Matchers}

class ScammanderSpec extends FunSuite with Matchers with ScammanderBaseAll {

  override type RootSender         = Unit
  override type RunExtra           = Unit
  override type TabExtra           = Unit
  override type ResultTpe          = Unit
  override type StaticChildCommand = DummyStaticChildCommand
  case class DummyStaticChildCommand(command: ComplexCommand) extends BaseStaticChildCommand

  override protected val defaultCommandSuccess: Unit = ()

  override protected def tabExtraToRunExtra(extra: Unit): Unit = ()

  override type G[A] = Either[CommandFailureNEL, A]

  type Parser[A] = StateT[G, List[RawCmdArg], A]
  implicit val E: ScammanderTypes.ParserError[Parser] = raiseInd(
    stateMonadLayerControl,
    handleEither
  )

  type ResultIO[A] = EitherT[IO, CommandFailureNEL, A]

  def ioState[S](start: S): IO[MonadState[IO, S]] =
    Ref[IO]
      .of(start)
      .map { ref =>
        new DefaultMonadState[IO, S] {
          override val monad: Monad[IO]    = IO.ioEffect
          override def get: IO[S]          = ref.get
          override def set(s: S): IO[Unit] = ref.set(s)
        }
      }

  case class MyObj(name: String, i: Int)

  val myObjName1             = "foo"
  val myObjName2             = "bar"
  val myObjName3             = "baz"
  val myObjects: List[MyObj] = List(MyObj(myObjName1, 1), MyObj(myObjName2, 2), MyObj(myObjName3, 3))

  implicit val myObjHasName: HasName[MyObj]      = (a: MyObj) => a.name
  implicit val myObjparam: Parameter[Set[MyObj]] = Parameter.mkNamed("myobj", myObjects)

  def mkArgs(arguments: String): List[RawCmdArg] = ScammanderHelper.stringToRawArgsQuoted(arguments)

  def parse[A](arguments: String, checkEmpty: Boolean = true)(implicit param: Parameter[A]): Option[A] = {
    val parsed = param.parse[Parser]((), ()).run(mkArgs(arguments))
    if (checkEmpty && arguments.nonEmpty) {
      parsed.foreach {
        case (xs, _) =>
          assert(xs.isEmpty)
      }
    }

    parsed.map(_._2).toOption
  }

  def error[A](arguments: String)(implicit param: Parameter[A]): CommandFailureNEL = {
    val parsed = param.parse[Parser]((), ()).run(mkArgs(arguments))
    assert(parsed.isLeft)
    parsed.left.get
  }

  def singleError[A](arguments: String)(implicit param: Parameter[A]): CommandFailure = {
    val nel = error[A](arguments)
    assert(nel.size == 1)
    nel.head
  }

  def usage[A](implicit param: Parameter[A]): String = {
    val e = param.usage[G](())
    assert(e.isRight)
    e.right.get
  }

  def suggestions[A](arguments: String)(implicit param: Parameter[A]): Seq[String] = {
    val either =
      ioState(mkArgs(arguments)).flatMap(implicit state => param.suggestions[ResultIO]((), ()).value).unsafeRunSync()
    assert(either.isRight)
    either.right.get
  }

  def noSuggestions[A](arguments: String)(implicit param: Parameter[A]): Assertion = {
    val either =
      ioState(mkArgs(arguments)).flatMap(implicit state => param.suggestions[ResultIO]((), ()).value).unsafeRunSync()
    assert(either.contains(Nil))
  }

  def errorSuggestions[A](arguments: String)(implicit param: Parameter[A]): Assertion = {
    val either =
      ioState(mkArgs(arguments)).flatMap(implicit state => param.suggestions[ResultIO]((), ()).value).unsafeRunSync()
    assert(either.isLeft)
  }
}
