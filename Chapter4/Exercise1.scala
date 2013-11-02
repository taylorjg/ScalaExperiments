import fpinscala.datastructures._

object Exercise1 {

	private def mean(xs: Seq[Double]): Option[Double] =
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	private def showResult(msg: String, r: Option[Double]): Unit =
		r match {
			case Some(v) => println(msg + "r = %f".format(v))
			case _ => println(msg + "r = None")
		}

	def main(args: Array[String]): Unit = {

		val v1 = Some(12)
		println(v1.get)

		val v2 = Some("A String value")
		println(v2.get)

		showResult("mean(List(...)): ", mean(List(2.0, 2.0, 2.0)))
		showResult("mean(Nil): ", mean(Nil))

		showResult("map(Some): ", Some(2.0).map(_ * 2))
		val v3: Option[Double] = None
		showResult("map(None): ", v3.map(_ * 2))
		showResult("map(None): ", (None:Option[Double]).map(_ * 2))

		showResult("flatMap(Some): ", Some(2.0).flatMap((x: Double) => Some(x * 4)))
		showResult("flatMap(None): ", Some(2.0).flatMap((x: Double) => None))
	}
}
