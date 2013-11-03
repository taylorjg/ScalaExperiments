import fpinscala.datastructures._

object Exercise5 {

	private def sequence1[A](as: List[Option[A]]): Option[List[A]] = {

		def loop[A](as: List[Option[A]], bs: List[A]): Option[List[A]] =
			as match {
				case head :: tail => head match {
					case Some(a) => loop(tail, a :: bs)
					case _ => None
				}
				case Nil => Some(bs.reverse)
			}

		loop(as, Nil)
	}

	private def sequence2[A](as: List[Option[A]]): Option[List[A]] =
		try {
			Some(as.foldRight(Nil: List[A])((ao: Option[A], bs: List[A]) => {
				ao match {
					case Some(a) => a :: bs
					case _ => throw new Exception("Found a None item")
				}
			}))
		}
		catch {
			case e: Exception => None
		}

	private def showResult[A](as: List[Option[A]]): Unit = {
		val r1 = sequence1(as)
		println("sequence1(%s): %s".format(as, r1))
		val r2 = sequence2(as)
		println("sequence2(%s): %s".format(as, r2))
	}

	def main(args: Array[String]): Unit = {

		showResult(List(Some(1), Some(2), Some(3)))
		showResult(List(Some(1.0), Some(2.0), Some(3.0)))
		showResult(List(Some("A"), Some("B"), Some("C")))

		showResult(Nil: List[Option[Int]])
		showResult(Nil: List[Option[Double]])
		showResult(Nil: List[Option[String]])

		showResult(List(None, Some(2), Some(3)))
		showResult(List(Some(1), None, Some(3)))
		showResult(List(Some(1), Some(2), None))
		showResult(List(None, None, None))
	}
}
