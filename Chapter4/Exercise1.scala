import fpinscala.datastructures._

object Exercise1 {

	private def mean(xs: Seq[Double]): Option[Double] =
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	private def showResult(msg: String, r: Any): Unit =
		println("%s: result = %s".format(msg, r.toString()))

	def main(args: Array[String]): Unit = {

		val v1 = Some(12)
		println(v1)
		println(v1.get)

		val v2 = Some(14.56)
		println(v2)
		println(v2.get)

		val v3 = Some("A string value")
		println(v3)
		println(v3.get)

		val v4 = None
		println(v4)

		showResult("mean(List(...))", mean(List(2.0, 2.0, 2.0)))
		showResult("mean(Nil)", mean(Nil))

		showResult("map(Some)", Some(2.0).map(_ * 2))
		showResult("map(None)", (None:Option[Double]).map(_ * 2))

		showResult("flatMap(Some)", Some(2.0).flatMap((x: Double) => Some(x * 4)))
		showResult("flatMap(None)", Some(2.0).flatMap((x: Double) => None))

		showResult("getOrElse(Some)", Some(2.0).getOrElse(4.0))
		showResult("getOrElse(None)", (None:Option[Double]).getOrElse(4.0))

		showResult("orElse(Some)", Some(2.0).orElse(Some(4.0)))
		showResult("orElse(None)", (None:Option[Double]).orElse(Some(4.0)))

		showResult("filter(Some)", Some(2.0).filter(_ > 1.0))
		showResult("filter(Some)", Some(2.0).filter(_ > 10.0))
		showResult("filter(None)", (None:Option[Double]).filter(_ > 1.0))
		showResult("filter(None)", (None:Option[Double]).filter(_ > 10.0))
	}
}
