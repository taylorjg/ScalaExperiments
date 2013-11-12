import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise3 {

	def main(args: Array[String]): Unit = {
		val rng = Simple(123)
		println(intDouble(rng))
		println(doubleInt(rng))
		println(double3(rng))
	}	
}
