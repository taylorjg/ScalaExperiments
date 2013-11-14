import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise7 {

	def main(args: Array[String]): Unit = {

		val rng = Simple(123)

		val (n1, rng2) = positiveEven(rng)
		val (n2, rng3) = positiveEven(rng2)
		val (n3, rng4) = positiveOdd(rng3)
		val (n4, rng5) = positiveOdd(rng4)
		println((n1, rng2))
		println((n2, rng3))
		println((n3, rng4))
		println((n4, rng5))

		val fs = List(positiveEven, positiveEven, positiveOdd, positiveOdd)
		val r = sequence(fs)(rng)
		println(r)
	}	
}
