package fpinscala.datastructures {

	case class State[S, +A](run: S => (A, S)) {

		def flatMap[B](f: A => State[S, B]): State[S, B] =
			State(s => {
				val (a, s2) = run(s)
				f(a).run(s2)
			})

		def map[B](f: A => B): State[S, B] =
			flatMap(a => State(s => (f(a), s)))

		def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
			flatMap(a => rb.flatMap(b => State(s => (f(a, b), s))))
	}

	object State {

		type Rand[A] = State[RNG, A]

		def unit[S, A](a: A): State[S, A] =
			State(s => (a, s))

		def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
			State(s => {
				def loop(fs: List[State[S, A]], s: S): (List[A], S) = {
					fs match {
						case hd::tl => {
 							val (a, s2) = hd.run(s)
							val t = loop(tl, s2)
							(a :: t._1, t._2)
						}
						case _ => (Nil, s)
					}
				}

				loop(fs, s)
			})
	}
}
