package fp.testing
import org.scalatest.WordSpec
import rng.RNG

class PropTest extends WordSpec {
  import fp.testing.Prop._

  "&&" when {
    "both result in passed cases" should {
      "create a Prop that passes" in {
        val prop1 = Prop((_, _) => Passed)
        val prop2 = Prop((_, _) => Passed)

        val result = prop1 && prop2

        assert(result.run(1, RNG.Simple(1)) == Passed)
      }
    }

    "the left results in a failed case" should {
      "create a Prop that will falsify with success count of the first" in {
        val failure = Falsified("bad", 5)
        val prop1 = Prop((_, _) => failure)
        val prop2 = Prop((_, _) => Passed)

        val result = prop1 && prop2

        assert(result.run(10, RNG.Simple(1)) === failure)
      }
    }

    "the right results in a failed case" should {
      "create a Prop that will falsify with success count of the second" in {
        val failure = Falsified("bad", 5)
        val prop1 = Prop((_, _) => Passed)
        val prop2 = Prop((_, _) => failure)

        val result = prop1 && prop2

        assert(result.run(10, RNG.Simple(1)) === failure)
      }
    }

    "the right and left results in a failed case" should {
      "create a Prop that will falsify with success count of the first" in {
        val failure = Falsified("bad", 5)
        val prop1 = Prop((_, _) => failure)
        val prop2 = Prop((_, _) => Falsified("oops", 3))

        val result = prop1 && prop2

        assert(result.run(10, RNG.Simple(1)) === failure)
      }
    }
  }

}
