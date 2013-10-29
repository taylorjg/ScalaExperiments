object Exercise2 {

	private def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n: Int): Boolean = 
			if (n >= as.length) true
			else if (f(as(n), as(n - 1))) loop(n + 1)
			else false

		loop(1)
	}

	private def tryArray[A](as: Array[A], f: (A, A) => Boolean): Unit = {
		val result = isSorted(as, f)
		val msg = "result is %b"
		println(msg.format(result));
	}

	def main(args: Array[String]): Unit = {

		// These should be true.
		tryArray(Array(1), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2, 3), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2, 2), (a: Int, b: Int) => a >= b);

		// These should be false.
		tryArray(Array(1, 0, 3), (a: Int, b: Int) => a >= b);
		tryArray(Array(1, 2, 1), (a: Int, b: Int) => a >= b);

		// These should be true.
		tryArray(Array(1.1), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2, 3.3), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2, 2.2), (a: Double, b: Double) => a >= b);

		// These should be false.
		tryArray(Array(1.1, 0.0, 3.3), (a: Double, b: Double) => a >= b);
		tryArray(Array(1.1, 2.2, 1.1), (a: Double, b: Double) => a >= b);
	}
}
