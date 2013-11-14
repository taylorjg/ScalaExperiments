import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise8 {
	def main(args: Array[String]): Unit = {
		val rng = Simple(41665789)
		val (n1, rng2) = positiveLessThan(6)(rng)
		val (n2, rng3) = positiveLessThan(6)(rng2)
		val (n3, rng4) = positiveLessThan(6)(rng3)
		println((n1, rng2))
		println((n2, rng3))
		println((n3, rng4))
	}	
}
