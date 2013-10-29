object Partial1 {

	private def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
		(b: B) => f(a, b)
	}

	def main(args: Array[String]): Unit = {
		val p = partial1(1.1, (a: Double, b: Int) => a * b)
		val c = p(4)
		val msg = "c = %f"
		println(msg.format(c))
	}
}
