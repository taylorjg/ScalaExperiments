import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise5 {

	def main(args: Array[String]): Unit = {

		val rng = Simple(456)

		val (n1, rng2) = positiveEven(rng)
		val (n2, rng3) = positiveEven(rng2)
		val (n3, rng4) = positiveEven(rng3)

		println(n1)
		println(n2)
		println(n3)
	}	
}
