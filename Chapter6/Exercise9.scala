import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise9 {
	def main(args: Array[String]): Unit = {

		val rng = Simple(123)

		val (n1a, rng2a) = map(positiveInt)(_ / 2.0)(rng)
		println((n1a, rng2a))

		val (n1b, rng2b) = mapV2(positiveInt)(_ / 2.0)(rng)
		println((n1b, rng2b))

		val (n2a, rng3a) = map2(positiveEven, positiveOdd)((a, b) => (b, a))(rng)
		println((n2a, rng3a))

		val (n2b, rng3b) = map2V2(positiveEven, positiveOdd)((a, b) => (b, a))(rng)
		println((n2b, rng3b))
	}	
}
