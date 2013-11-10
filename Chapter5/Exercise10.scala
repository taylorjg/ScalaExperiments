import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise10 {

	def main(args: Array[String]): Unit = {
		val s = fibs.take(25).toList
		println(s)
	}	
}
