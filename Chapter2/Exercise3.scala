object Exercise3 {

	private def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
		(b: B) => f(a, b)
	}

	private def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
		(a: A) => partial1(a, f)
	}

	def main(args: Array[String]): Unit = {
		val beans = (a: Double, b: Int) => a * b
		val curriedBeans = curry(beans)
		val partiallyAppliedBeans = curriedBeans(1.1)
		val c = partiallyAppliedBeans(4)
		val msg = "c = %f"
		println(msg.format(c))
	}	
}
