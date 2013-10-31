package fpinscala.datastructures {

	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {
		
		// Exercise 2
		def mytail[A](as: List[A]): List[A] = as match {
			case Nil => Nil
			case Cons(h, t) => t
		}

		// Exercise 3
		def setHead[A](as: List[A], a: A): List[A] =
			Cons(a, as)

		// Exercise 4
		def mydrop[A](as: List[A], n: Int): List[A] = {
			if (n == 0) as
			else as match {
				case Nil => Nil
				case Cons(h, t) => mydrop(t, n - 1)
			}
		}

		// Exercise 5
		def mydropWhile[A](as: List[A], p: A => Boolean): List[A] = {
			as match {
				case Nil => Nil
				case Cons(h, t) => if (p(h)) mydropWhile(t, p) else as
			}
		}

		// Exercise 6
		def myinit[A](as: List[A]): List[A] = {

			def loop1[A](as1: List[A], as2: List[A]): List[A] = {
				as1 match {
					case Nil => as2
					case Cons(_, Nil) => as2
					case Cons(h, t) => loop1(t, Cons(h, as2))
				}
			}

			def loop2[A](as1: List[A], as2: List[A]): List[A] = {
				as1 match {
					case Nil => as2
					case Cons(h, t) => loop2(t, Cons(h, as2))
				}
			}

			loop2(loop1(as, Nil), Nil)
		}

		def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}

		def product(ds: List[Double]): Double = ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x, xs) => x * product(xs)
		}

		def apply[A](as: A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))

	}
}
