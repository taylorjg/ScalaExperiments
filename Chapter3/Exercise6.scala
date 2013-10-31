import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise6 {
	def main(args: Array[String]): Unit = {

		val list1 = List(1, 2, 3, 4, 5)
		println(list1)
		println(myinit(list1))
		
		println(myinit(Nil))
	}
}
