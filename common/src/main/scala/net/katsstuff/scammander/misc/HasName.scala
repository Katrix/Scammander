package net.katsstuff.scammander.misc

/**
  * Proof that a type has a name.
  */
trait HasName[A] {

  /**
    * Get the name of this object.
    */
  def apply(a: A): String
}
object HasName {
  def apply[A](obj: A)(implicit hasName: HasName[A]): String     = hasName(obj)
  def apply[A](implicit hasName: HasName[A]):         HasName[A] = hasName
}
