object Exercise2 {

	trait Monoid[A] {
		def op(a1: A, a2: A): A
		def zero: A
	}

	def optionMonoid[A] = new Monoid[Option[A]] {
		def op(a1: Option[A], a2: Option[A]) = a1 match {
			case Some(_) => a1
			case None => a2
		}
		val zero = None
	}

	def main(args: Array[String]): Unit = {
	}
}
