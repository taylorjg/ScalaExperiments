package fpinscala.datastructures {

	trait RNG {
		def nextInt: (Int, RNG)
	}

	case class Simple(seed: Long) extends RNG {
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = Simple(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}

	object RNG {

		def positiveInt(rng: RNG): (Int, RNG) = {
			val (n1, nextRNG) = rng.nextInt
			val n2 = if (n1 == Int.MinValue) Int.MaxValue else if (n1 < 0) -n1 else n1
			(n2, nextRNG)
		}

		def double(rng: RNG): (Double, RNG) = {
			val (n, nextRNG) = positiveInt(rng)
			val d = n.toDouble / (Int.MaxValue.toDouble + 1)
			(d, nextRNG)
		}

		def intDouble(rng: RNG): ((Int, Double), RNG) = {
			val (n, rng2) = positiveInt(rng)
			val (d, rng3) = double(rng2)
			((n, d), rng3)
		}

		def doubleInt(rng: RNG): ((Double, Int), RNG) = {
			val (d, rng2) = double(rng)
			val (n, rng3) = positiveInt(rng2)
			((d, n), rng3)
		}

		def double3(rng: RNG): ((Double, Double, Double), RNG) = {
			val (d1, rng2) = double(rng)
			val (d2, rng3) = double(rng2)
			val (d3, rng4) = double(rng3)
			((d1, d2, d3), rng4)
		}

		def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
			def loop(count: Int, rng: RNG): (List[Int], RNG) = {
				if (count <= 0) (Nil, rng)
				else {
					val (n, rng2) = rng.nextInt
					val t = loop(count - 1, rng2)
					(n :: t._1, t._2)
				}
			}

			loop(count, rng)
		}

		type Rand[+A] = RNG => (A, RNG)

		val int1: Rand[Int] = (rng: RNG) => rng.nextInt
		val int2: Rand[Int] = (rng) => rng.nextInt
		val int3: Rand[Int] = rng => rng.nextInt
		val int: Rand[Int] = _.nextInt

		def unit[A](a: A): Rand[A] =
			rng => (a, rng)

		def map[A, B](stateAction: Rand[A])(f: A => B): Rand[B] =
			rng => {
				val (a, rng2) = stateAction(rng)
				(f(a), rng2)
			}

		// ra = a state action that yields (A, RNG)
		// rb = a state action that yields (B, RNG)
		def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
			rng => {
				val (a, rng2) = ra(rng)
				val (b, rng3) = rb(rng2)
				val c = f(a, b)
				(c, rng3)
			}
		}

		def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
			rng => {
				def loop(fs: List[Rand[A]], rng: RNG): (List[A], RNG) = {
					fs match {
						case hd::tl => {
							val (a, rng2) = hd(rng)
							val t = loop(tl, rng2)
							(a :: t._1, t._2)
						}
						case _ => (Nil, rng)
					}
				}

				loop(fs, rng)
			}

		def positiveEven: Rand[Int] =
			map(positiveInt)(i => i - i % 2)

		def positiveOdd: Rand[Int] =
			map(positiveInt)(i => i - i / 2)

		def doubleV2(rng: RNG): (Double, RNG) =
			map(positiveInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))(rng)

		def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
			map2(ra, rb)((_, _))

		def randIntDouble: Rand[(Int, Double)] =
			both(int, double)

		def randDoubleInt: Rand[(Double, Int)] =
			both(double, int)

		def intsV2(count: Int)(rng: RNG): (List[Int], RNG) = {
			val fs = List.fill(count)(int)
			sequence(fs)(rng)
		}

		def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
			rng => {
				val (a, rng2) = f(rng)
				g(a)(rng2)
			}

		def positiveLessThan(n: Int): Rand[Int] =
			flatMap(positiveInt)(i => {
				rng => {
					val mod = i % n
					if (i + (n - 1) - mod > 0) (mod, rng)
					else positiveLessThan(n)(rng)
				}
			})

		def mapV2[A, B](ra: Rand[A])(f: A => B): Rand[B] =
			flatMap(ra)(a => rng => (f(a), rng))

		def map2V2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
			flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))

		def rollDieBad: Rand[Int] = positiveLessThan(6)

		def rollDieGood: Rand[Int] = {
			val f = positiveLessThan(6)
			map(f)(_ + 1)
		}
	}
}
