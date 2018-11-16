package fp.parallel

import java.util.concurrent.ExecutorService

import org.scalatest.FunSuite

import scala.util.Random

class ParTest extends FunSuite {

  val es: ExecutorService = java.util.concurrent.Executors.newCachedThreadPool()

  test("testAsyncF") {
    val r = new Random

    val f: Int => Int = a => {
      println("start " + a)
      Thread.sleep(r.nextInt(1000))
      println("finish " + a)
      a + 10
    }

    val asyncFn = Par.asyncF(f)
    val r1 = asyncFn(5)(es)
    val r2 = asyncFn(6)(es)

    assert(15 == r1.get())
    assert(16 == r2.get())
  }

  test("sum using fold") {
    assert(15 === Par.fold(1 to 5)(0)(_ + _)(es).get())
  }

  test("max using fold") {
    assert(10 === Par.fold((1 to 10) ++ (-4 to 5))(Int.MinValue)(Math.max)(es).get())
  }

  test("min using fold") {
    assert(-4 === Par.fold((1 to 10) ++ (-4 to 5))(Int.MaxValue)(Math.min)(es).get())
  }

//  test("total words") {
//    assert(9 === Par.totalWords(IndexedSeq("hi there my friend yo wassup", "yo yo yo"))(es).get())
//  }

}
