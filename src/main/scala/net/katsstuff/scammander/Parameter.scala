package net.katsstuff.scammander

import scala.util.Try

import net.katsstuff.scammander.misc.MkHListWitness
import shapeless._
import shapeless.labelled.FieldType

trait Parameter[RootSender, A] {

  def name: String

  def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)]

  def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String])

  def usage: String = s"<$name>"
}
trait ProxyParameter[RootSender, A, B] extends Parameter[RootSender, A] {
  def param: Parameter[RootSender, B]

  override def name: String = param.name

  override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) =
    param.suggestions(source, xs)

  override def usage: String = param.usage
}

object Parameter {
  def apply[RootSender, A](implicit tpe: Parameter[RootSender, A]): Parameter[RootSender, A] = tpe
}

trait ParameterInstances[RootSender]
    extends NormalParametersInstances[RootSender]
    with ParameterModifierInstances[RootSender]
    with ParameterLabelledDeriver[RootSender]

trait NormalParametersInstances[RootSender] {
  def primitivePar[A](parName: String, s: String => A): Parameter[RootSender, A] =
    new Parameter[RootSender, A] {
      override def name: String = parName

      override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)] =
        if (xs.nonEmpty) Try(s(xs.head)).map(xs.tail -> _).toEither.left.map(_.getMessage)
        else Left("Not enough parameters")

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs.tail, Nil)
    }

  def mkSingle[A](
      parName: String,
      parser: String => Either[String, A],
      possibleSuggestions: () => Seq[String]
  ): Parameter[RootSender, A] =
    new Parameter[RootSender, A] {
      override def name: String = parName

      override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)] =
        if (xs.nonEmpty) parser(xs.head).map(xs.tail -> _)
        else Left("Not enough parameters")

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val head = xs.head
        val tail = xs.tail

        if (tail.isEmpty) (Nil, possibleSuggestions().filter(head.startsWith)) else (tail, Nil)
      }
    }

  implicit val bytePar:   Parameter[RootSender, Byte]    = primitivePar("byte", _.toByte)
  implicit val shortPar:  Parameter[RootSender, Short]   = primitivePar("short", _.toShort)
  implicit val intPar:    Parameter[RootSender, Int]     = primitivePar("int", _.toInt)
  implicit val longPar:   Parameter[RootSender, Long]    = primitivePar("long", _.toLong)
  implicit val floatPar:  Parameter[RootSender, Float]   = primitivePar("float", _.toFloat)
  implicit val doublePar: Parameter[RootSender, Double]  = primitivePar("double", _.toDouble)
  implicit val boolPar:   Parameter[RootSender, Boolean] = primitivePar("boolean", _.toBoolean)
  implicit val strPar:    Parameter[RootSender, String]  = primitivePar("string", identity)
}

trait ParameterModifierInstances[RootSender] {

  case class Named[S <: String, A](param: Parameter[RootSender, A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[RootSender, A, A] {
    override def name: String = w.value

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)] =
      param.parse(source, xs)
  }

  case class Choices(name: String, choices: Set[String], sendValid: Boolean = false)
      extends Parameter[RootSender, String] {
    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], String)] = {
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

  case class ChoicesT[Name <: String, L <: HList, SendValid <: Boolean](
      implicit nameW: Witness.Aux[Name],
      mkHList: MkHListWitness[L],
      toTraversable: ops.hlist.ToTraversable.Aux[L, Set, String],
      sendValidW: Witness.Aux[SendValid]
  ) extends ProxyParameter[RootSender, String, String] {
    private val choices: Set[String] = toTraversable(mkHList.value)

    override val param: Parameter[RootSender, String] = Choices(nameW.value, choices, sendValidW.value)

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], String)] =
      param.parse(source, xs)
  }

  case class NeedPermission[S <: String, A](param: Parameter[RootSender, A])(implicit w: Witness.Aux[S])
      extends ProxyParameter[RootSender, A, A] {
    val perm: String = w.value

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)] =
      if (hasSenderPermission(source, perm)) param.parse(source, xs)
      else Left("You do not have the permissions needed to use this parameter")
  }

  def hasSenderPermission(sender: RootSender, permission: String): Boolean

  case class OnlyOne[A](param: Parameter[RootSender, Seq[A]]) extends ProxyParameter[RootSender, A, A] {

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], A)] =
      param.parse(source, xs).flatMap {
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
trait ParameterLabelledDeriver[RootSender] extends ParameterDeriver[RootSender] {

  implicit def hConsLabelledParam[HK <: Symbol, HV, T <: HList](
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[RootSender, HV]],
      tParam: Lazy[Parameter[RootSender, T]]
  ): Parameter[RootSender, FieldType[HK, HV] :: T] =
    new Parameter[RootSender, FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name} ${tParam.value.name}"

      override def parse(
          source: RootSender,
          xs: List[String]
      ): Either[String, (List[String], FieldType[HK, HV] :: T)] = {
        for {
          (ys, h) <- hParam.value.parse(source, xs)
          (_, t)  <- tParam.value.parse(source, ys)
        } yield (Nil, labelled.field[HK](h) :: t)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }

  implicit def cConsLabelledParam[HK <: Symbol, HV, T <: HList](
      hName: Witness.Aux[HK],
      hParam: Lazy[Parameter[RootSender, HV]],
      tParam: Lazy[Parameter[RootSender, T]]
  ): Parameter[RootSender, FieldType[HK, HV] :: T] =
    new Parameter[RootSender, FieldType[HK, HV] :: T] {
      override def name: String = s"${hName.value.name}|${tParam.value.name}"

      override def parse(
          source: RootSender,
          xs: List[String]
      ): Either[String, (List[String], FieldType[HK, HV] :: T)] = {
        for {
          (ys, h) <- hParam.value.parse(source, xs)
          (_, t)  <- tParam.value.parse(source, ys)
        } yield (Nil, labelled.field[HK](h) :: t)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }
}

trait ParameterDeriver[RootSender] {
  implicit def hConsParam[H, T <: HList](
      hParam: Lazy[Parameter[RootSender, H]],
      tParam: Lazy[Parameter[RootSender, T]]
  ): Parameter[RootSender, H :: T] =
    new Parameter[RootSender, H :: T] {
      override def name: String = s"${hParam.value.name} ${tParam.value.name}"

      override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], H :: T)] = {
        for {
          (ys, h) <- hParam.value.parse(source, xs)
          (_, t)  <- tParam.value.parse(source, ys)
        } yield (Nil, h :: t)
      }

      override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = {
        val (ys, h)   = hParam.value.suggestions(source, xs)
        val (rest, t) = tParam.value.suggestions(source, ys)

        (rest, h ++ t)
      }
    }

  implicit val hNilParam: Parameter[RootSender, HNil] = new Parameter[RootSender, HNil] {
    override def name: String = ""

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], HNil)] =
      if (xs.isEmpty) Right((Nil, HNil))
      else Left(s"Too many arguments:\n${xs.mkString(", ")}")

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs, Nil)
  }

  implicit def cConsParam[H, T <: Coproduct](
      implicit hParam: Lazy[Parameter[RootSender, H]],
      tParam: Lazy[Parameter[RootSender, T]]
  ): Parameter[RootSender, H :+: T] =
    new Parameter[RootSender, H :+: T] {
      override def name: String = s"${hParam.value.name}|${tParam.value.name}"

      override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], H :+: T)] = {
        hParam.value.parse(source, xs).map { case (ys, h) => ys -> Inl(h) }.left.flatMap { e1 =>
          tParam.value.parse(source, xs).map { case (ys, t) => ys -> Inr(t) }.left.map { e2 =>
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

  implicit val cNilParam: Parameter[RootSender, CNil] = new Parameter[RootSender, CNil] {
    override def name: String = ""

    override def parse(source: RootSender, xs: List[String]): Either[String, (List[String], CNil)] =
      sys.error("CNil")

    override def suggestions(source: RootSender, xs: List[String]): (List[String], Seq[String]) = (xs, Nil)
  }
}
