object Exercise3 {

	trait Monoid[A] {
		def op(a1: A, a2: A): A
		def zero: A
	}

	def endoMonoid[A] = new Monoid[A => A] {
		def op(a1: A => A, a2: A => A) = a2 compose a1
		def zero = identity
	}

	def main(args: Array[String]): Unit = {
	}
}
