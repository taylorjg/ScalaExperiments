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
