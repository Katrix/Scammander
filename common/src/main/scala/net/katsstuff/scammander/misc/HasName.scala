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
