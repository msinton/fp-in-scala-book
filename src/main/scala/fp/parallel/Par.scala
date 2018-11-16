package fp.parallel

import java.util.concurrent.{Callable, ExecutorService, Future}

object Par {

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val r1 = a(es)
    val r2 = b(es)
    UnitFuture(f(r1.get, r2.get))
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((x, _) => f(x))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def run[A](executorService: ExecutorService)(a: Par[A]): Future[A] = a(executorService)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(Nil))((pa, acc) => map2(pa, acc)(_ :: _))
  }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as map asyncF(f))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map asyncF((a: A) => if (f(a)) List(a) else Nil)
    map(sequence(pars))(_.flatten)
  }

  def fold[A](as: Seq[A])(unitVal: A)(f: (A, A) => A): Par[A] = {
    if (as.lengthCompare(1) <= 0) unit(as.headOption.getOrElse(unitVal))
    else {
      val (l, r) = as.splitAt(as.size / 2)
      map2(fork(fold(l)(unitVal)(f)), fork(fold(r)(unitVal)(f)))(f)
    }
  }

  // something is not right with this?
  def agg[A, B](as: Seq[A])(unitVal: B)(f: (A, B) => B)(g: (B, B) => B): Par[B] = {
    as match {
      case Nil => unit(unitVal)
      case a :: Nil => unit(f(a, unitVal))
      case _ =>
        val (l, r) = as.splitAt(as.size / 2)
        map2(fork(agg(l)(unitVal)(f)(g)), fork(agg(r)(unitVal)(f)(g)))(g)
    }
  }

  def totalWords(ws: Seq[String]): Par[Int] =
    agg[String, Int](ws)(0)((a, b) => b + a.split(" ").length)(_ + _)
}