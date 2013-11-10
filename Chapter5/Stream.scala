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

		def takeWhile(p: A => Boolean): Stream[A] = {
			def loop(s: Stream[A], p: A => Boolean, s2: Stream[A]): Stream[A] =
				s.uncons match {
					case Some(c) if p(c.head) => Stream.cons(c.head, loop(c.tail, p, s2))
					case _ => s2
				}

			loop(this, p, Empty)
		}

		def exists(p: A => Boolean): Boolean =
			uncons match {
				case Some(c) => p(c.head) || c.tail.exists(p)
				case _ => false
			}

		def foldRight[B](z: => B)(f: (A, => B) => B): B = 
			uncons match {
				case Some(c) => f(c.head, c.tail.foldRight(z)(f))
				case _ => z
			}

		def exists2(p: A => Boolean): Boolean =
			foldRight(false)((a, b) => p(a) || b)

		def forAll(p: A => Boolean): Boolean =
			uncons match {
				case Some(c) => p(c.head) && c.tail.forAll(p)
				case _ => true
			}

		def takeWhile2(p: A => Boolean): Stream[A] =
			foldRight(Empty: Stream[A])((a, foldedTail) => if (p(a)) Stream.cons(a, foldedTail) else Empty)

		def map[B](f: A => B): Stream[B] =
			foldRight(Empty: Stream[B])((a, foldedTail) => Stream.cons(f(a), foldedTail))

		def filter(p: A => Boolean): Stream[A] =
			foldRight(Empty: Stream[A])((a, foldedTail) => {
				if (p(a)) Stream.cons(a, foldedTail)
				else foldedTail
			})

		def append[B >: A](b: B): Stream[B] = 
			foldRight(Stream(b))((a, foldedTail) => Stream.cons(a, foldedTail))

		def flatMap[B](f: A => Stream[B]): Stream[B] = {
			foldRight(Empty: Stream[B])((a, foldedTail) => {
				def loop(s: Stream[B], t: Stream[B]): Stream[B] = {
					s.uncons match {
						case Some(c) => Stream.cons(c.head, loop(c.tail, t))
						case _ => t
					}
				}
				loop(f(a), foldedTail)
			})
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

		def constant[A](a: A): Stream[A] =
			cons(a, constant(a))
	}
}
