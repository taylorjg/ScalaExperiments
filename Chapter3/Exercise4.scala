import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise4 {

	def main(args: Array[String]): Unit = {
		val list1 = List(1, 2, 3, 4, 5)
		println(list1)
		println(mydrop(list1, 3))
	}
}
