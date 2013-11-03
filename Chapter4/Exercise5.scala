import fpinscala.datastructures._

object Exercise5 {

	private def sequence[A](as: List[Option[A]]): Option[List[A]] = {
		None
	}

	private def showResult[A](as: List[Option[A]]): Unit = {
		val r = sequence(as)
		println("sequence(%s): %s".format(as, r))
	}

	def main(args: Array[String]): Unit = {
		showResult(List(Some(1), Some(2), Some(3)))
		showResult(List(None, Some(2), Some(3)))
		showResult(List(Some(1), None, Some(3)))
		showResult(List(Some(1), Some(2), None))
		showResult(List(None, None, None))
	}
}
