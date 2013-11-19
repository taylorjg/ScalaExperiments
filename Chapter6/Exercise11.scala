import fpinscala.datastructures._
import fpinscala.datastructures.State._

object Exercise11 {

	def main(args: Array[String]): Unit = {

		sealed trait Input
		case object Coin extends Input
		case object Turn extends Input

		case class Machine(locked: Boolean, candies: Int, coins: Int) {
			def processInput(input: Input): ((Int, Int), Machine) = {

				var newLocked = locked
				var newCandies = candies
				var newCoins = coins

				if (candies > 0) {
					input match {
						case Coin if locked => {
							newLocked = false
							newCoins = coins + 1
						}
						case Turn if !locked => {
							newLocked = true
							newCandies = candies - 1	
						}
					}
				}

				val newMachine = Machine(newLocked, newCandies, newCoins)
				((newCoins, newCandies), newMachine)
			}
		}

		def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
			inputs match {
				case hd::tl => {
					State(s => {
						val t = s.processInput(hd)
						simulateMachine(tl).run(t._2)
					})
				}
				case _ => State(s => ((s.coins, s.candies), s))
			}
		}

		val machine = Machine(true, 5, 10)
		val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
		println(machine)
		println(inputs)
		println(simulateMachine(inputs).run(machine))
	}	
}
