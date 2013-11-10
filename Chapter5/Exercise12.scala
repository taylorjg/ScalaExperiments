import fpinscala.datastructures._
import fpinscala.datastructures.Stream._

object Exercise12 {

	def main(args: Array[String]): Unit = {

		println("ones2.take(9).toList: %s".format(ones2.take(9).toList))
		
		println("constant2(23).take(10).toList: %s".format(constant2(23).take(10).toList))
		println("constant2(12.7).take(10).toList: %s".format(constant2(12.7).take(10).toList))
		println("constant2(\"oxo\").take(10).toList: %s".format(constant2("oxo").take(10).toList))

		println("from2(16).take(10).toList: %s".format(from2(16).take(10).toList))

		println("fibs2.take(20).toList: %s".format(fibs2.take(20).toList))
	}	
}
