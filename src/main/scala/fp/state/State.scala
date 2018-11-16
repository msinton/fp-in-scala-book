package fp.state

import fp.state.State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def mapWrittenWithFlatMap[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

}


object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def loop(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case f :: fs => f.run(s) match {
          case (a, s1) => loop(s1, fs, a :: acc)
        }
      }

    State(s => loop(s, sas, Nil))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // modify is the way to express something that transforms the state without any output
  // In the rng example this would be used to set a new seed
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}
