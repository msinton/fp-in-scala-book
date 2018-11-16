package fp.testing.gen

import org.scalatest.WordSpec
import rng.RNG

class GenTest extends WordSpec {

  "Gen" should {

    "choose - check proportion of values created" in {

      val (randSeq, _) = Gen.streamOfN(1000)(Gen.choose(0, 2))
        .sample.run(RNG.Simple(1))

      val countsMap = randSeq.groupBy(identity).mapValues(_.size)

      val zeroCount = countsMap(0)
      val oneCount = countsMap(1)

      assert(zeroCount == 500)
      assert(oneCount == 500)
    }
  }

  "scale by range - simple" in {
    val scaler = Gen.scaleByRange(0, 8, 0, 4) _
    assert(0.0 === scaler(0))
    assert(0.5 === scaler(1))
    assert(1.0 === scaler(2))

    assert(3.0 === scaler(6))
    assert(3.5 === scaler(7))
    assert(4.0 === scaler(8))
  }

  "scale by range - start not 0" in {
    val scaler = Gen.scaleByRange(0, 8, 1, 5) _
    assert(1.0 === scaler(0))
    assert(1.5 === scaler(1))
    assert(2.0 === scaler(2))
    assert(5.0 === scaler(8))
  }

  "scale by range - prev start not 0" in {
    val scaler = Gen.scaleByRange(-1, 7, 1, 5) _
    assert(1.0 === scaler(-1))
    assert(1.5 === scaler(0))
    assert(2.0 === scaler(1))
    assert(5.0 === scaler(7))
  }

  "scale by range - reverse" in {
    val scaler = Gen.scaleByRange(0, 8, 4, 0) _
    assert(4.0 === scaler(0))
    assert(3.5 === scaler(1))
    assert(3.0 === scaler(2))

    assert(1.0 === scaler(6))
    assert(0.5 === scaler(7))
    assert(0.0 === scaler(8))
  }
}
