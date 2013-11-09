import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise2 {

	private def showResult[A](sIn: Stream[A], n: Int): Unit = {
		val sOut = sIn.take(n)
		println("%s.take(%d): %s".format(sIn.toList, n, sOut.toList))
	}

	def main(args: Array[String]): Unit = {
		showResult(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 6)
		showResult(Stream(1, 2, 3, 4), 6)
		showResult(Empty, 6)
	}	
}
