import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise2 {

	def main(args: Array[String]): Unit = {

		val list1 = List(1, 2, 3, 4, 5)
		println(list1)

		val list2 = mytail(list1)
		println(list2)

		val list3 = mytail(list2)
		println(list3)
	}
}
