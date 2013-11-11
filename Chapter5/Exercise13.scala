import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise13 {

	def main(args: Array[String]): Unit = {
		val s1 = Stream(1, 2, 3, 4, 5)
		println("%s.map2(_ * 2): %s".format(s1.toList, s1.map2(_ * 2).toList))
		println("%s.map2(_ / 2.0): %s".format(s1.toList, s1.map2(_ / 2.0).toList))
	}	
}
