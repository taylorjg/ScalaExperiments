import fpinscala.datastructures._
import fpinscala.datastructures.Option._

object Exercise3 {
	
	private def showResult[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Unit = {
		val c = map2(a, b)(f(_, _))
		println("map2(%s, %s): %s".format(a, b, c))
	}

	def main(args: Array[String]): Unit = {
		showResult(Some(1), Some(2))(_ + _)
		showResult(Some(2.0), Some(4))(_ * _)
		showResult(Some(1), None:Option[Int])(_ + _)
		showResult(None:Option[Int], Some(2))(_ + _)
	}
}
