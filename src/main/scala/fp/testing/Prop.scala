package fp.testing

import fp.testing.Prop._

trait Prop {

  def check: Either[FailedCase, SuccessCount]
  def &&(p: Prop): Prop

}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}


trait Prop1 {

  def check: Boolean
  def &&(p: Prop1): Prop1

}

case class SimpleProp(f: () => Boolean) extends Prop1 {
  override def check: Boolean = f()

  override def &&(p: Prop1): Prop1 = SimpleProp(() => f() && p.check)
}