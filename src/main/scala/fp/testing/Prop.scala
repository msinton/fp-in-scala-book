package fp.testing

import fp.testing.Prop._
import fp.testing.gen.Gen
import rng.RNG

import scala.util.Try

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p2: Prop): Prop =
    Prop((cases, rng) =>
      run(cases, rng) match {
        case Passed       => p2.run(cases, rng)
        case x: Falsified => x
    })

  def ||(p2: Prop): Prop =
    Prop((cases, rng) =>
      run(cases, rng) match {
        case Passed                => Passed
        case Falsified(failure, _) => p2.tag(failure).run(cases, rng)
    })

  def tag(msg: String) =
    Prop((cases, rng) =>
      run(cases, rng) match {
        case Falsified(failure, successCount) => Falsified(msg + "\n" + failure, successCount)
        case x                                => x
    })
}

object Prop {
  type FailedCase   = String
  type SuccessCount = Int
  type TestCases    = Int
  type Tag          = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def randomStream[A](g: Gen[A])(rng: RNG): fp.laziness.Stream[A] =
    fp.laziness.Stream.unfold(rng)(rng => Option(g.sample.run(rng)))

  def buildMsg[A](a: A, e: Throwable): String =
    s"""Test case $a
       |generated an exception ${e.getMessage}
       |stack trace: ${e.getStackTrace.mkString("\n")}
    """.stripMargin

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (n, rng) =>
      randomStream(as)(rng)
        .zip(fp.laziness.Stream.from(0))
        .take(n)
        .map {
          case (a, index) =>
            Try { if (f(a)) Passed else Falsified(a.toString, index) }
              .fold(e => Falsified(buildMsg(a, e), index), identity)
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
  )

}
