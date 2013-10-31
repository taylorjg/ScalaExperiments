import fpinscala.datastructures._
import fpinscala.datastructures.List._

object Exercise1 {

	private def X(): Int = {
		val x = List(1, 2, 3, 4, 5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101
		}
		x
	}

	def main(args: Array[String]): Unit = {
		val x = X()
		val msg = "x = %d"
		// I think x will be 3.
		println(msg.format(x))
	}
}
