import fpinscala.datastructures._
import fpinscala.datastructures.RNG._

object Exercise1 {

	def main(args: Array[String]): Unit = {
		val rng = Simple(123)
		println(positiveInt(rng))
	}	
}
