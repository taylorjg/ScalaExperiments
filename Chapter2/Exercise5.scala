object Exercise5 {

	private def compose[A, B, C](f: B => C, g: A => B): A => C = {
		(a: A) => f(g(a))
	}

	def main(args: Array[String]): Unit = {
		val f = (b: Int) => b * b
		val g = (a: Int) => a + 2
		val c = compose(f, g)(3)
		val msg = "c = %d"
		println(msg.format(c))
	}
}
