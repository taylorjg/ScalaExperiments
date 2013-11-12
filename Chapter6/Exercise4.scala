import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise4 {

	def main(args: Array[String]): Unit = {

		val rng = Simple(123)

		val (n1, rng2) = rng.nextInt
		val (n2, rng3) = rng2.nextInt
		val (n3, rng4) = rng3.nextInt
		println(n1)
		println(n2)
		println(n3)

		val (ns, _) = ints(3)(rng)
		println(ns)
	}	
}
