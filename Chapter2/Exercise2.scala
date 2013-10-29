object Exercise2 {

	private def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n: Int): Boolean = 
			if (n >= as.length) true
			else if (f(as(n), as(n - 1)) == false) false
			else loop(n + 1)

		loop(1)
	}

	private def tryArray[A](as: Array[A], f: (A, A) => Boolean): Unit = {
		val result = isSorted(as, f)
		val msg = "result is %b"
		println(msg.format(result));
	}

	def main(args: Array[String]): Unit = {

		tryArray(Array(1, 2, 3), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2, 2), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2, 1), (a: Int, b: Int) => a >= b);

		tryArray(Array(1.1, 2.2, 3.3), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2, 2.2), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2, 1.1), (a: Double, b: Double) => a >= b);
	}
}
