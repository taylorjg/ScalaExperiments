import fpinscala.datastructures._

object Exercise1 {

	private def mean(xs: Seq[Double]): Option[Double] =
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	private def showResult(r: Option[Double]): Unit =
		r match {
			case Some(v) => println("r = %f".format(v))
			case _ => println("r = None!")
		}

	def main(args: Array[String]): Unit = {

		val v1 = Some(12)
		println(v1.get)

		val v2 = Some("A String value")
		println(v2.get)

		showResult(mean(List(2.0, 2.0, 2.0)))
		showResult(mean(Nil))

		showResult(Some(2.0).map(_ * 2))
		val v3: Option[Double] = None
		showResult(v3.map(_ * 2))
		showResult((None:Option[Double]).map(_ * 2))
	}
}
