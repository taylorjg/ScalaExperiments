import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise13 {

	def main(args: Array[String]): Unit = {

		val s1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

		println("%s.map2(_ * 2): %s".format(s1.toList, s1.map2(_ * 2).toList))
		println("%s.map2(_ / 2.0): %s".format(s1.toList, s1.map2(_ / 2.0).toList))

		println("%s.take2(5): %s".format(s1.toList, s1.take2(5).toList))
	}	
}
