import Prop._

object Prop {

	type FailedCase = String
	type SuccessCount = Int
	type TestCases = Int

	// https://gist.github.com/aloiscochard/1334040
	private def unfold[S, A](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s) match {
		case Some((a, s2)) => a #:: unfold(s2)(f)
		case None => Stream.Empty
	}	

	private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    	// Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    	unfold(rng)(rng => Some(g.sample.run(rng)))
	}

	private def buildMsg[A](a: A, e: Exception): String =
		s"test case: $a\n" +
		s"generated an exception: ${e.getMessage}\n" +
		s"stack trace:\n ${e.getStackTrace.mkString("\n")}"	

	def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
		(n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
			case (a, i) => try {
				if (f(a)) Passed else Falsified(a.toString, i)
			} catch { case e: Exception => Falsified(buildMsg(a, e), i) }
		}.find(_.isFalsified).getOrElse(Passed)
	}
}

sealed trait Result {
	def isFalsified: Boolean
}

case object Passed extends Result {
	def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
	def isFalsified = true
}

case class Prop(run: (TestCases, RNG) => Result) {

	// Exercise 9
	def &&(p: Prop): Prop = Prop {
		(n, rng) => {
			this.run(n, rng) match {
				case Passed => p.run(n, rng)
				case f => f
			}
		}
	}

	// Exercise 9
	def ||(p: Prop): Prop = Prop {
		(n, rng) => {
			this.run(n, rng) match {
				case Passed => Passed
				case f => p.run(n, rng)
			}
		}
	}
}

case class Gen[A](sample: State[RNG, A]) {

	// Exercise 6
	def flatMap[B](f: A => Gen[B]): Gen[B] = {
		Gen(State {
			rng => {
				val (a, rng2) = this.sample.run(rng)
				val g2 = f(a)
				g2.sample.run(rng2)
			}
		})
	}

	// Exercise 6
	def listOfN(size: Gen[Int]): Gen[List[A]] = {
		size.flatMap(n => Gen.listOfN(n, this))
	}
}

object Gen {

	// Exercise 4
	def choose(start: Int, stopExclusive: Int): Gen[Int] =
		Gen(State {
			rng => {
				val (n, rng2) = RNG.positiveInt(rng)
				val rangeSize = stopExclusive - start + 1
				val n2 = (n % rangeSize) + start
				(n2, rng2)
			}
		})

	// Exercise 5
	def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

	// Exercise 5
	def boolean: Gen[Boolean] =
		Gen(State {
			rng => {
				val g = choose(0, 1)
				g.sample.run(rng) match {
					case (0, rng2) => (false, rng2)
					case (1, rng2) => (true, rng2)
				}
			}
		})

	// Exercise 5
	def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
		def loop(n: Int, rng: RNG): (List[A], RNG) = {
			if (n <= 0) (Nil, rng)
			else {
				val (a, rng2) = g.sample.run(rng)
				val (as, rng3) = loop(n - 1, rng2)
				(a :: as, rng3)
			}
		}
		Gen(State {
			rng => loop(n, rng)
		})
	}

	// Exercise 7
	def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
		val g = choose(0, 1)
		g.flatMap(n => n match {
			case 0 => g1
			case 1 => g2
		})
	}

	// Exercise 8
	def weighted[A](g1: (Gen[A],Int), g2: (Gen[A],Int)): Gen[A] = {
		val (gen1, w1) = g1
		val (gen2, w2) = g2
		val g = choose(0, w1 + w2)
		g.flatMap(n => n match {
			case x if x <= w1 => gen1
			case _ => gen2
		})
	}
}

object PropertyBasedTesting {

	def main(args: Array[String]): Unit = {

		val rng = Simple(679345999 + 3)

		val g1 = Gen.choose(1, 10)
		val s1 = g1.sample.run(rng)
		println(s"s1 = $s1")

		val g2 = Gen.boolean
		val s2 = g2.sample.run(rng)
		println(s"s2 = $s2")

		val g3 = Gen.listOfN(10, g1)
		val s3 = g3.sample.run(rng)
		println(s"s3 = $s3")

		val g4 = g2.listOfN(g1)
		val s4 = g4.sample.run(rng)
		println(s"s4 = $s4")

		val g5 = Gen.union(Gen.unit("abc"), Gen.unit("def"))
		val s5 = g5.sample.run(rng)
		println(s"s5 = $s5")

		val g6 = Gen.union(Gen.unit("abc"), Gen.unit("def")).listOfN(g1)
		val s6 = g6.sample.run(rng)
		println(s"s6 = $s6")

		val g7 = Gen.weighted((Gen.unit("abc"), 1), (Gen.unit("def"), 3)).listOfN(g1)
		val s7 = g7.sample.run(rng)
		println(s"s7 = $s7")

		val prop1 = Prop.forAll(g1) {
			n => {
				println(s"n = $n")
				n >= 1 && n <= 10
			}
		}
		prop1.run(10, rng)
	}
}
