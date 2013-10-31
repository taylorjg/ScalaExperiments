import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise3 {

	def main(args: Array[String]): Unit = {
		val list1 = List(1.1, 2.2, 3.3, 4.4, 5.5)
		println(list1)
		println(setHead(list1, 9.9))
	}
}
