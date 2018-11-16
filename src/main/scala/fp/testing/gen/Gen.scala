package fp.testing.gen

import fp.state.State
import fp.testing.Prop
import rng.RNG

case class Gen[A](sample: State[RNG, A]) {

}

object Gen {

  def sequence[A](actions: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequence(actions.map(_.sample)))

  // straight forward approach but doesn't work well for v large range that starts in negatives
  // because doing mod (%) with Int too big?
  def chooseSimple(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(rng => RNG.nonNegativeInt(rng))
      .map(start + _ % (stopExclusive - start)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](State(rng => {
      val (a, rngNext) = rng.nextInt

      val interpolatedValue = scaleByRange(Int.MinValue, Int.MaxValue, start, stopExclusive - 1)(a)
      (Math.round(interpolatedValue).toInt, rngNext)
    }))
  }

  def scaleByRange(prevMin: Double, prevMax: Double, min: Double, max: Double)(valueInOldRange: Double): Double = {
    val newRange = max - min
    val scale = newRange / (prevMax - prevMin)
    (valueInOldRange - prevMin) * scale + min
  }

  def listOfN[A](n: Int)(a: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(a))

  def lazySequence[S, A](sas: Stream[State[S, A]]): State[S, Stream[A]] = {
    def loop(s: S, actions: Stream[State[S, A]], acc: Stream[A]): (Stream[A], S) =
      actions match {
        case Stream() => (acc.reverse, s)
        case f #:: fs => f.run(s) match {
          case (a, s1) => loop(s1, fs, a #:: acc)
        }
      }

    State(s => loop(s, sas, Stream()))
  }

  def streamOfN[A](n: Int)(a: Gen[A]): Gen[Stream[A]] =
    Gen(lazySequence(Stream.fill(n)(a).map(_.sample)))

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???


}
