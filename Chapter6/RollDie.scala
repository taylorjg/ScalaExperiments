import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object RollDie {
	def main(args: Array[String]): Unit = {

		val rng1 = Simple(123)
		val rng2 = Simple(888)
		val rng3 = Simple(14141)

		val (n1a, _) = rollDieBad(rng1)
		val (n2a, _) = rollDieBad(rng2)
		val (n3a, _) = rollDieBad(rng3)
		println(n1a)
		println(n2a)
		println(n3a)

		val (n1b, _) = rollDieGood(rng1)
		val (n2b, _) = rollDieGood(rng2)
		val (n3b, _) = rollDieGood(rng3)
		println(n1b)
		println(n2b)
		println(n3b)
		
	}	
}
