import fpinscala.datastructures._

object Exercise2 {

	private def mean(xs: Seq[Double]): Option[Double] =
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

	private def variance(xs: Seq[Double]): Option[Double] =
		mean(xs).flatMap((m: Double) => mean(xs.map((x: Double) => math.pow(x - m, 2))))

	private def showResult(msg: String, r: Any): Unit =
		println("%s: result = %s".format(msg, r.toString()))

	def main(args: Array[String]): Unit = {
		val xs = List(1.0, 2.0, 3.0)
		showResult("mean(List(1.0, 2.0, 3.0))", mean(xs))
		showResult("variance(List(1.0, 2.0, 3.0))", variance(xs))
	}
}
