import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise1 {

	def main(args: Array[String]): Unit = {

		val s1 = Stream(1, 2, 3, 4)
		println("s1.toList(): %s".format(s1.toList))

		val s2 = Empty
		println("s2.toList(): %s".format(s2.toList))
	}
}
