import fpinscala.datastructures._

object Exercise7 {

	def main(args: Array[String]): Unit = {

		val e1 = Right(12)
		val e2 = Left("Blah")

		println("%s map: %s".format(e1, e1.map(_ * 2)))
		println("%s map: %s".format(e2, e2.map(_.toString())))

		println("%s flatMap: %s".format(e1, e1.flatMap(x => Right(x * 4))))
		println("%s flatMap: %s".format(e1, e1.flatMap(x => Left("left"))))
		println("%s flatMap: %s".format(e2, e2.flatMap(x => Left("left"))))

		println("%s orElse: %s".format(e1, e1.orElse(Left("left"))))
		println("%s orElse: %s".format(e1, e1.orElse(Right("right"))))
		println("%s orElse: %s".format(e2, e2.orElse(Left("left"))))
		println("%s orElse: %s".format(e2, e2.orElse(Right("right"))))

		println("%s map2: %s".format(e1, e1.map2(Right(13))(_ + _)))
		println("%s map2: %s".format(e1, e1.map2(Left("left"))((x, y) => "")))
		println("%s map2: %s".format(e2, e2.map2(Left("left"))((x, y) => "")))
	}
}
