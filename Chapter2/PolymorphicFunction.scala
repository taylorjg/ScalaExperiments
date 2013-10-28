object PolymorphicFunction {

	def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
		@annotation.tailrec
		def loop(n: Int): Int = 
			if (n >= ds.length) -1
			else if (p(ds(n))) n
			else loop(n + 1)

		loop(0)
	}

	def main(args: Array[String]): Unit = {
		val index = findFirst(Array(1, 2, 3), (x: Int) => x == 2)
		val msg = "index is %d"
		println(msg.format(index));
	}
}
