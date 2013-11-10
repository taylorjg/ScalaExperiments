import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise9 {

	def main(args: Array[String]): Unit = {
		val s = from(42).take(13).toList
		println(s)
	}	
}
