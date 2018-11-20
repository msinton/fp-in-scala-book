package fp.laziness

import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def toList: List[A] = {
    val acc = ListBuffer.empty[A]
    @tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        acc += h()
        loop(t())
      case _ => acc.toList
    }
    loop(this)
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(),h2()), (t(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_, _))

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty[B])((h,t) => cons(f(h), t))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    Cons(() => head, () => tail)
  }

  def unfold[A,S](start: S)(f: S => Option[(A,S)]): Stream[A] = f(start) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case _ => Empty
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

}
