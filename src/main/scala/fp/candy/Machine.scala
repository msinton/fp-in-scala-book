package fp.candy

import fp.state._
import fp.state.State._

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def update: Input => Machine => Machine = input => machine =>
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Coin, Machine(true, c, x)) => Machine(locked = false, c, x + 1)
      case (Turn, Machine(false, c, x)) => Machine(locked = true, c - 1, x)
    }

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State((m0: Machine) => {
      val machine = inputs.foldLeft(m0) { case (m, input) => update(input)(m) }
      ((machine.candies, machine.coins), machine)
    })
  }

  // remember State[Machine, (Int,Int)] means it has run:
  // machine => ((Int,Int), newMachine)
  def simulate2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    // _ <- sequence(inputs map (i => modify[Machine](update(i))))   // this is equivalent ^
    s <- get
  } yield (s.candies, s.coins)


}