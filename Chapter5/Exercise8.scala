import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise8 {

	def main(args: Array[String]): Unit = {
		val s = constant(42).take(13).toList
		println(s)
	}	
}
