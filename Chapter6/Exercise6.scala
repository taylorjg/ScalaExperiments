import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise6 {

	def main(args: Array[String]): Unit = {

		val rng = Simple(123)
		
		println(intDouble(rng))
		println(randIntDouble(rng))

		println(doubleInt(rng))
		println(randDoubleInt(rng))
	}	
}
