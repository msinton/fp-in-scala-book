package candy

import org.scalatest.WordSpec

class MachineTest extends WordSpec {

  "simulate" when {

    "Machine is locked with 5 candies" when {
      val machine = Machine(locked = true, 5, 0)

      "Turn" should {
        val actions = List(Turn)

        "not change" in {
          val result = Machine.simulate(actions).run(machine)
          assert(result == ((5, 0), machine))
        }
      }

      "insert Coin" should {
        val actions = List(Coin)

        "unlock" in {
          val result = Machine.simulate(actions).run(machine)
          assertState(result)(locked = false, candies = 5, coins = 1)
        }
      }

      "get 2 candies" should {
        val actions = List(Coin, Turn, Coin, Turn)

        "have 3 left and contain 2 coins" in {
          val result = Machine.simulate(actions).run(machine)
          assertState(result)(locked = true, candies = 3, coins = 2)
        }
      }

      "get 6 candies" should {
        val actions = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

        "have 0 left and contain 5 coins" in {
          val result = Machine.simulate(actions).run(machine)
          assertState(result)(locked = true, candies = 0, coins = 5)
        }
      }
    }
  }

  private def assertState(result: ((Int, Int), Machine))(locked: Boolean, candies: Int, coins: Int) =
    assert(result === ((candies, coins), Machine(locked, candies, coins)))
}
