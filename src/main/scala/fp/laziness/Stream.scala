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

}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    Cons(() => head, () => tail)
  }

  def unfold[A](p: A => Boolean)(start: A): Stream[A] = ???

}
