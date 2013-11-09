package fpinscala.datastructures {

	sealed abstract class Stream[+A] {
		def uncons: Option[Cons[A]]
		def isEmpty: Boolean = uncons.isEmpty

		def toList: List[A] = {
			def loop(s: Stream[A], as: List[A]): List[A] = {
				s.uncons match {
					case Some(c) => c.head :: loop(c.tail, as)
					case _ => as
				}
			}

			loop(this, Nil)
		}

		def take(n: Int): Stream[A] = {
			def loop(s: Stream[A], remaining: Int, s2: Stream[A]): Stream[A] =
				if (remaining <= 0) s2
				else {
					s.uncons match {
						case Some(c) => Stream.cons(c.head, loop(c.tail, remaining - 1, s2))
						case _ => s2
					}
				}

			loop(this, n, Empty)
		}
	}

	object Empty extends Stream[Nothing] {
		val uncons = None
	}

	sealed abstract class Cons[+A] extends Stream[A] {
		def head: A
		def tail: Stream[A]
		val uncons = Some(this)
	}

	object Stream {

		def empty[A]: Stream[A] = Empty

		def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
			lazy val head = hd
			lazy val tail = tl
		}

		def apply[A](as: A*): Stream[A] = 
			if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
	}
}
