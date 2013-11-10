import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise5 {

	private def showResult[A](sIn: Stream[A], p: A => Boolean): Unit = {
		val sOut = sIn.takeWhile2(p)
		println("%s.takeWhile2(): %s".format(sIn.toList, sOut.toList))
	}

	def main(args: Array[String]): Unit = {
		val p = (x: Int) => x < 7
		showResult(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), p)
		showResult(Stream(1, 2, 3, 4), p)
		showResult(Empty, p)
	}	
}
