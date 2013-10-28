object Exercise1 {

	private def fibonacci(n: Int): Int = {

		/*
			e.g. when n = 6

			go(6, 0, 1)
			go(5, 1, 1)
			go(4, 1, 2)
			go(3, 2, 3)
			go(2, 3, 5)
			go(1, 5, 8)
			go(0, 8, 13)

		*/

		@annotation.tailrec
		def go(n: Int, a: Int, b: Int): Int = {
			if (n <= 0) a
			else go(n - 1, b, a + b)
		}

		if (n < 2) n
		else go(n, 0, 1)
	}

	private def fibonacciMsg(n: Int): String = {
		val msg = "Fibonacci of %d is %d"
		msg.format(n, fibonacci(n))
	}

	def main(args: Array[String]): Unit = {
		@annotation.tailrec
		def loop(n: Int, max: Int): Unit = {
			if (n <= max) {
				println(fibonacciMsg(n))
				loop(n + 1, max)
			}
		}
		
		loop(0, 12)
	}
}
