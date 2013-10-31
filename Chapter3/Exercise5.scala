import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise5 {
	def main(args: Array[String]): Unit = {

		val list1 = List(1, 2, 3, 4, 5)

		println(list1)
		println(mydropWhile(list1, (a: Int) => a <= 3))
		
		println(list1)
		println(mydropWhile(list1, (a: Int) => a > 10))
	}
}
