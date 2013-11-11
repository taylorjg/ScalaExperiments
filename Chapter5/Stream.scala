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

		def map2[B](f: A => B): Stream[B] =
			Stream.unfold(this)(s => {
				s.uncons match {
					case Some(c) => Some(f(c.head), c.tail)
					case _ => None
				}
			})

		def take2(n: Int): Stream[A] = {
			val z = (this, n)
			Stream.unfold(z)(state => {
				val (s, n) = state
				if (n <= 0) None
				else s.uncons match {
					case Some(c) => Some((c.head, (c.tail, n - 1)))
					case _ => None
				}
			})
		}

		def takeWhile3(p: A => Boolean): Stream[A] =
			Stream.unfold(this)(s => {
				s.uncons match {
					case Some(c) if p(c.head) => Some((c.head, c.tail))
					case _ => None
				}
			})

		def zip[B](s2: Stream[B]): Stream[(A, B)] =
			Stream.unfold((this, s2))(s => {
				for {
					c1 <- s._1.uncons
					c2 <- s._2.uncons
				} yield ((c1.head, c2.head), (c1.tail, c2.tail))
			})

		/*
		s1.uncons match {
			case Some(c1) =>
				s2.uncons match {
					case Some(c2) => Some(((c1.head, c2.head), (c1.tail, c2.tail)))
					case _ => None
				}
			case _ => None
		}
		*/

		def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
			Stream.unfold((this, s2))(s => {
				val (s1, s2) = s
				s1.uncons match {
					case Some(c1) =>
						s2.uncons match {
							case Some(c2) => Some(((Some(c1.head), Some(c2.head)), (c1.tail, c2.tail)))
							case _ => None
						}
					case _ => None
				}
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

		def from(n: Int): Stream[Int] =
			cons(n, from(n + 1))

		def fibs: Stream[Int] = {
			def loop(n: Int, lastButOne: Int, last: Int): Stream[Int] = {
				val next = if (n < 2) n else lastButOne + last
				cons(next, loop(n + 1, last, next))
			}

			loop(0, 0, 1)
		}

		// Exercise 11
		def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
			f(z) match {
				case Some((a, s)) => {
					cons(a, unfold(s)(f))
				}
				case _ => Empty
			}
		}

		def ones2: Stream[Int] =
			unfold(0)(_ => Some((1, 0)))

		def constant2[A](a: A): Stream[A] =
			unfold(0)(_ => Some((a, 0)))

		def from2(n: Int): Stream[Int] =
			unfold(n)(s => Some((s, s + 1)))

		def fibs2: Stream[Int] = {
			val z = (0, 0, 1)
			unfold(z)(s => {
				val (n, lastButOne, last) = s
				val next = if (n < 2) n else lastButOne + last
				Some((next, (n + 1, last, next)))
			})
		}
	}
}
