package net.katsstuff.scammander

trait SenderTransformer[RootSender, A] {

  def validate(sender: RootSender): Either[CmdError, A]

  def toSender(a: A): RootSender
}
object SenderTransformer {
  def mkTransformer[RootSender, A](
      validator: RootSender => Either[CmdError, A]
  )(back: A => RootSender): SenderTransformer[RootSender, A] = new SenderTransformer[RootSender, A] {
    override def validate(sender: RootSender): Either[CmdError, A] = validator(sender)
    override def toSender(a: A):               RootSender          = back(a)
  }
}
