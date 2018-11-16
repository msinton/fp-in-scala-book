package rng

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    (if (i >= 0) i else -(i + 1), nextRng)
  }

  // a value in [0, 1) - inclusive of 0, exclusive of 1
  def double(rng: RNG): (Double, RNG) = {
    val (natNum, nextRng) = nonNegativeInt(rng)
    (natNum / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, nextRng) = rng.nextInt
    (i % 2 == 0, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    intDouble(rng) match {
      case ((i, d), r) => ((d, i), r)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (Seq[Int], RNG) = {
    def loop(acc: Seq[Int], n: Int, r: RNG): (Seq[Int], RNG) = {
      if (n <= 0) (acc, r) else {
        val (i, r2) = r.nextInt
        loop(acc :+ i, i - 1, r2)
      }
    }

    loop(Seq(), count, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: Seq[Rand[A]]): Rand[Seq[A]] =
    map(
      fs.foldLeft[Rand[Seq[A]]](unit(Seq()))((acc, f) => map2(acc, f)(_ :+ _))
    )(_.reverse)


  def _ints(count: Int): Rand[Seq[Int]] =
    sequence(Seq.fill(count)(int))

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = s(rng)
    f(a)(r1)
  }

}