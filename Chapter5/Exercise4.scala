import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise4 {

	// private def showResult[A](sIn: Stream[A], p: A => Boolean): Unit = {
	// 	val sOut = sIn.takeWhile(p)
	// 	println("%s.takeWhile(): %s".format(sIn.toList, sOut.toList))
	// }

	def main(args: Array[String]): Unit = {

		val s = Stream(1, 2, 3, 4, 5)

		println("%s.exists(_ == 3): %s".format(s.toList, s.exists(_ == 3)))
		println("%s.exists(_ == 8): %s".format(s.toList, s.exists(_ == 8)))

		println("%s.exists2(_ == 3): %s".format(s.toList, s.exists2(_ == 3)))
		println("%s.exists2(_ == 8): %s".format(s.toList, s.exists2(_ == 8)))

		println("%s.forAll(_ < 10): %s".format(s.toList, s.forAll(_  < 10)))
		println("%s.forAll(_ < 3): %s".format(s.toList, s.forAll(_  < 3)))
	}	
}
