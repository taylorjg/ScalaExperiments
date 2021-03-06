object Exercise1 {

	trait Monoid[A] {
		def op(a1: A, a2: A): A
		def zero: A
	}

	val stringMonoid = new Monoid[String] {
		def op(a1: String, a2: String) = a1 + a2
		val zero = ""
	}

	def listMonoid[A] = new Monoid[List[A]] {
		def op(a1: List[A], a2: List[A]) = a1 ++ a2
		val zero = Nil
	}

	val intAddition = new Monoid[Int] {
		def op(a1: Int, a2: Int) = a1 + a2
		val zero = 0
	}

	val intMultiplication = new Monoid[Int] {
		def op(a1: Int, a2: Int) = a1 * a2
		val zero = 1
	}

	val booleanOr = new Monoid[Boolean] {
		def op(a1: Boolean, a2: Boolean) = a1 || a2
		val zero = false
	}

	val booleanAnd = new Monoid[Boolean] {
		def op(a1: Boolean, a2: Boolean) = a1 && a2
		val zero = true
	}

	def main(args: Array[String]): Unit = {
	}
}
