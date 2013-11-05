import fpinscala.datastructures._

object Exercise8 {

	private def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
 		as.foldRight[Either[E, List[B]]](Right(Nil))((a, e) => f(a).map2(e)(_ :: _))

	private def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
		traverse(es)(identity)

	def main(args: Array[String]): Unit = {

		val l1 = List(1, 2, 3)
		val r1 = traverse(l1)(x => Right(x * 2))
		println("traverse(%s): %s".format(l1, r1))

		val l2 = List(Right(1), Right(2), Right(3))
		val r2 = sequence(l2)
		println(r2)

		val l3 = List(Right(1), Left("Blah"), Right(3))
		val r3 = sequence(l3)
		println(r3)

		val l4 = List(Right(1), Left("Blah1"), Left("Blah2"))
		val r4 = sequence(l4)
		println(r4)
	}
}
