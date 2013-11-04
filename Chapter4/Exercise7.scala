import fpinscala.datastructures._

object Exercise7 {

	private def showResult[A](as: List[Option[A]]): Unit = {
	}

	def main(args: Array[String]): Unit = {

		val e1 = Right(12)
		val e2 = Left("Blah")

		println("%s map: %s".format(e1, e1.map(_ * 2)))
		println("%s map: %s".format(e2, e2.map(_.toString())))

		println("%s flatMap: %s".format(e1, e1.flatMap(x => Right(x * 4))))
		println("%s flatMap: %s".format(e1, e1.flatMap(x => Left("left"))))
		println("%s flatMap: %s".format(e2, e2.flatMap(x => Left("left"))))
	}
}
