package net.katsstuff.scammander.misc

import shapeless._

trait MkHListWitness[L <: HList] {
  def value: L
}
object MkHListWitness {
  implicit def mkCons[H, T <: HList](implicit w: Witness.Aux[H], tMk: MkHListWitness[T]): MkHListWitness[H :: T] = new MkHListWitness[H :: T] {
    override def value: H :: T = w.value :: tMk.value
  }

  implicit val mkHnil: MkHListWitness[HNil] = new MkHListWitness[HNil] {
    override def value: HNil = HNil
  }
}
