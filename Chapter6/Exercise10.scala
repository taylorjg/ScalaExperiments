import fpinscala.datastructures._
import fpinscala.datastructures.RNG._
import fpinscala.datastructures.State._

object Exercise10 {

	def main(args: Array[String]): Unit = {

		val rng = Simple(123)

		// unit
		println(RNG.unit(34)(rng))
		println(State.unit(34).run(rng))

		val f1 = State(RNG.positiveEven)
		val f2 = State(RNG.positiveOdd)
		val t1 = f1.run(rng)
		val t2 = f2.run(t1._2)
		println(t1)
		println(t2)
		println(t1._1 + t2._1)

		// flatMap
		println(RNG.flatMap(RNG.positiveEven)(a => rng => {
			val (b, rng2) = RNG.positiveOdd(rng)
			(a + b, rng2)
		})(rng))

		println(f1.flatMap(a => State(s => {
			var (b, s2) = f2.run(s)
			(a + b, s2)
		})).run(rng))

		// map
		println(RNG.map(RNG.positiveEven)(_ / 10.0)(rng))
		println(State(RNG.positiveEven).map(_ / 10.0).run(rng))

		// map2
		println(RNG.map2(RNG.positiveEven, RNG.positiveOdd)(_ + _)(rng))
		println(f1.map2(f2)(_ + _).run(rng))

		// sequence
		val fs1 = List(positiveEven, positiveEven, positiveOdd, positiveOdd)
		val r1 = RNG.sequence(fs1)(rng)
		println(r1)
		val fs2 = List(
			State(RNG.positiveEven),
			State(RNG.positiveEven),
			State(RNG.positiveOdd),
			State(RNG.positiveOdd))
		val r2 = State.sequence(fs2).run(rng)
		println(r2)
	}	
}
