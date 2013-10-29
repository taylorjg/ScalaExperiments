object Exercise4 {

	private def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	def main(args: Array[String]): Unit = {
		val f = (a: Int) => (b: Double) => a * b
		val c = uncurry(f)(4, 9)
		val msg = "c = %f"
		println(msg.format(c))
	}
}
