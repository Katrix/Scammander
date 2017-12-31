package net.katsstuff.scammander

import scala.util.Try

import net.katsstuff.scammander.misc.MkHListWitness
import shapeless.labelled.FieldType
import shapeless._

trait ScammanderUniverse[RootSender, CmdCtx]
    extends NormalParametersInstances[RootSender, CmdCtx]
    with ParameterLabelledDeriver[RootSender, CmdCtx] {

  trait SenderTransformer[A] {

    def validate(sender: RootSender): Either[CmdError, A]

    def toSender(a: A): RootSender
  }
  object SenderTransformer {
    def mkTransformer[A](validator: RootSender => Either[CmdError, A])(back: A => RootSender): SenderTransformer[A] =
      new SenderTransformer[A] {
        override def validate(sender: RootSender): Either[CmdError, A] = validator(sender)
        override def toSender(a: A):               RootSender          = back(a)
      }
  }

  sealed trait CmdResult
  case class CmdError(msg: String)  extends CmdResult
  case class CmdSuccess(count: Int) extends CmdResult

  abstract class Command[Sender, Param](
      implicit val senderTransformer: SenderTransformer[Sender],
      val par: Parameter[Param]
  ) {

    def run(source: Sender, cmdCtx: CmdCtx, arg: Param): CmdResult

    def suggestions(source: Sender, strArgs: List[String]): Seq[String] =
      par.suggestions(senderTransformer.toSender(source), strArgs)._2

    def usage(source: Sender): String = par.usage
  }
  object Command {
    def simple[Sender, Param](
        runCmd: (Sender, CmdCtx, Param) => CmdResult
    )(implicit transformer: SenderTransformer[Sender], parameter: Parameter[Param]): Command[Sender, Param] =
      new Command[Sender, Param] {
        override def run(source: Sender, cmdCtx: CmdCtx, arg: Param): CmdResult = runCmd(source, cmdCtx, arg)
      }
  }

  trait Parameter[A] {

    def name: String

    def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)]

    def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String])

    def usage: String = s"<$name>"
  }
  trait ProxyParameter[A, B] extends Parameter[A] {
    def param: Parameter[B]

    override def name: String = param.name

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) =
      param.suggestions(source, xs)

    override def usage: String = param.usage
  }

  object Parameter {
    def apply[A](implicit tpe: Parameter[A]): Parameter[A] = tpe
  }

  case class Named[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S]) extends ProxyParameter[A, A] {
    override def name: String = w.value

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)] =
      param.parse(source, ctx, xs)
  }

  case class Choices(name: String, choices: Set[String], sendValid: Boolean = false) extends Parameter[String] {
    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], String)] = {
      if (xs.nonEmpty) {
        val head = xs.head
        if (choices.contains(head)) Right((xs.tail, head))
        else {
          if (sendValid) Left(s"$head is not a valid parameter.\nValid parameters: ${choices.mkString(", ")}")
          else Left(s"$head is not a valid parameter.")
        }
      } else Left("Not enough parameters")
    }

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
      val head = xs.head
      val tail = xs.tail

      if (tail.isEmpty) (Nil, choices.filter(head.startsWith).toSeq) else (tail, Nil)
    }
  }

  class ChoicesT[Name <: String, L <: HList, SendValid <: Boolean](
      implicit nameW: Witness.Aux[Name],
      mkHList: MkHListWitness[L],
      toTraversable: ops.hlist.ToTraversable.Aux[L, Set, String],
      sendValidW: Witness.Aux[SendValid]
  ) extends ProxyParameter[String, String] {
    private val choices: Set[String] = toTraversable(mkHList.value)

    override val param: Parameter[String] = Choices(nameW.value, choices, sendValidW.value)

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], String)] =
      param.parse(source, ctx, xs)
  }

  case class NeedPermission[S <: String, A](param: Parameter[A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[A, A] {
    val perm: String = w.value

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)] =
      if (hasSenderPermission(source, perm)) param.parse(source, ctx, xs)
      else Left("You do not have the permissions needed to use this parameter")
  }

  def hasSenderPermission(sender: RootSender, permission: String): Boolean

  case class OnlyOne[A](param: Parameter[Seq[A]]) extends ProxyParameter[A, Seq[A]] {

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)] =
      param.parse(source, ctx, xs).flatMap {
        case (rest, seq) if seq.size == 1 => Right((rest, seq.head))
        case _                            => Left("More than one possible value")
      }
  }

  class AllOff[A]
  /*TODO: Find way to use Option class instead
    class Optional[A]
    class OptionalWeak[A]
 */
}

trait NormalParametersInstances[RootSender, CmdCtx] { self: ScammanderUniverse[RootSender, CmdCtx] =>
  def primitivePar[A](parName: String, s: String => A): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)] =
        if (xs.nonEmpty) Try(s(xs.head)).map(xs.tail -> _).toEither.left.map(_.getMessage)
        else Left("Not enough parameters")

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs.tail, Nil)
    }

  def mkSingle[A](
      parName: String,
      parser: String => Either[String, A],
      possibleSuggestions: () => Seq[String]
  ): Parameter[A] =
    new Parameter[A] {
      override def name: String = parName

      override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], A)] =
        if (xs.nonEmpty) parser(xs.head).map(xs.tail -> _)
        else Left("Not enough parameters")

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val head = xs.head
        val tail = xs.tail

        if (tail.isEmpty) (Nil, possibleSuggestions().filter(head.startsWith)) else (tail, Nil)
      }
    }

  implicit val bytePar:   Parameter[Byte]    = primitivePar("byte", _.toByte)
  implicit val shortPar:  Parameter[Short]   = primitivePar("short", _.toShort)
  implicit val intPar:    Parameter[Int]     = primitivePar("int", _.toInt)
  implicit val longPar:   Parameter[Long]    = primitivePar("long", _.toLong)
  implicit val floatPar:  Parameter[Float]   = primitivePar("float", _.toFloat)
  implicit val doublePar: Parameter[Double]  = primitivePar("double", _.toDouble)
  implicit val boolPar:   Parameter[Boolean] = primitivePar("boolean", _.toBoolean)
  implicit val strPar:    Parameter[String]  = primitivePar("string", identity)
}

trait ParameterLabelledDeriver[RootSender, CmdCtx] extends ParameterDeriver[RootSender, CmdCtx] {
  self: ScammanderUniverse[RootSender, CmdCtx] =>

  implicit def hConsLabelledParam[HK <: Symbol, HV, T <: HList](
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :: T] =
    new Parameter[FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name} ${tParam.value.name}"

      override def parse(
          source: RootSender,
          ctx: CmdCtx,
          xs: List[String]
      ): Either[String, (List[String], FieldType[HK, HV] :: T)] = {
        for {
          t1 <- hParam.value.parse(source, ctx, xs)
          t2 <- tParam.value.parse(source, ctx, t1._1)
        } yield (Nil, labelled.field[HK](t1._2) :: t2._2)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }

  implicit def cConsLabelledParam[HK <: Symbol, HV, T <: HList](
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[HV]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[FieldType[HK, HV] :: T] =
    new Parameter[FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name}|${tParam.value.name}"

      override def parse(
          source: RootSender,
          ctx: CmdCtx,
          xs: List[String]
      ): Either[String, (List[String], FieldType[HK, HV] :: T)] = {
        for {
          t1 <- hParam.value.parse(source, ctx, xs)
          t2 <- tParam.value.parse(source, ctx, t1._1)
        } yield (Nil, labelled.field[HK](t1._2) :: t2._2)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }
}

trait ParameterDeriver[RootSender, CmdCtx] { self: ScammanderUniverse[RootSender, CmdCtx] =>
  implicit def hConsParam[H, T <: HList](hParam: Lazy[Parameter[H]], tParam: Lazy[Parameter[T]]): Parameter[H :: T] =
    new Parameter[H :: T] {
      override def name: String = s"${hParam.value.name} ${tParam.value.name}"

      override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], H :: T)] = {
        for {
          t1 <- hParam.value.parse(source, ctx, xs)
          t2 <- tParam.value.parse(source, ctx, t1._1)
        } yield (Nil, t1._2 :: t2._2)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }

  implicit val hNilParam: Parameter[HNil] = new Parameter[HNil] {
    override def name: String = ""

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], HNil)] =
      if (xs.isEmpty) Right((Nil, HNil))
      else Left(s"Too many arguments:\n${xs.mkString(", ")}")

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs, Nil)
  }

  implicit def cConsParam[H, T <: Coproduct](
      implicit hParam: Lazy[Parameter[H]],
      tParam: Lazy[Parameter[T]]
  ): Parameter[H :+: T] =
    new Parameter[H :+: T] {
      override def name: String = s"${hParam.value.name}|${tParam.value.name}"

      override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], H :+: T)] = {
        hParam.value.parse(source, ctx, xs).map { case (ys, h) => ys -> Inl(h) }.left.flatMap { e1 =>
          tParam.value.parse(source, ctx, xs).map { case (ys, t) => ys -> Inr(t) }.left.map { e2 =>
            s"$e1\n$e2"
          }
        }
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (hRest, h) = hParam.value.suggestions(source, xs)
        val (tRest, t) = tParam.value.suggestions(source, xs)

        val rest = if (hRest.size > tRest.size) hRest else tRest
        (rest, h ++ t)
      }
    }

  implicit val cNilParam: Parameter[CNil] = new Parameter[CNil] {
    override def name: String = ""

    override def parse(source: RootSender, ctx: CmdCtx, xs: List[String]): Either[String, (List[String], CNil)] =
      sys.error("CNil")

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs, Nil)
  }
}
