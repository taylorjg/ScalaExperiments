import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise7 {

	def main(args: Array[String]): Unit = {

		val s1 = Stream(1, 2, 3, 4, 5)

		println("%s.map(_ * 2): %s".format(s1.toList, s1.map(_ * 2).toList))
		println("%s.map(_ / 2.0): %s".format(s1.toList, s1.map(_ / 2.0).toList))

		println("%s.filter(_ %% 2 == 0): %s".format(s1.toList, s1.filter(_ % 2 == 0).toList))

		println("%s.append(15): %s".format(s1.toList, s1.append(15).toList))

		println("%s.flatMap(): %s".format(s1.toList, s1.flatMap(x => Stream(x, x * 2, x * 3)).toList))
	}	
}
