import Exercise4._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object MonoidSpecification extends Properties("Monoid") {

	//def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

	property("monoidLaws for stringMonoid") = forAll { (x: String, y: String, z: String) =>
		stringMonoid.op(stringMonoid.op(x, y), z) == stringMonoid.op(x, stringMonoid.op(y, z)) &&
		stringMonoid.op(x, stringMonoid.zero) == x &&
		stringMonoid.op(stringMonoid.zero, x) == x
	}

	property("monoidLaws for listMonoid") = forAll { (x: List[Int], y: List[Int], z: List[Int]) =>
		listMonoid.op(listMonoid.op(x, y), z) == listMonoid.op(x, listMonoid.op(y, z)) &&
		listMonoid.op(x, listMonoid.zero) == x &&
		listMonoid.op(listMonoid.zero, x) == x
	}

	property("monoidLaws for intAddition") = forAll { (x: Int, y: Int, z: Int) =>
		intAddition.op(intAddition.op(x, y), z) == intAddition.op(x, intAddition.op(y, z)) &&
		intAddition.op(x, intAddition.zero) == x &&
		intAddition.op(intAddition.zero, x) == x
	}

	property("monoidLaws for intMultiplication") = forAll { (x: Int, y: Int, z: Int) =>
		intMultiplication.op(intMultiplication.op(x, y), z) == intMultiplication.op(x, intMultiplication.op(y, z)) &&
		intMultiplication.op(x, intMultiplication.zero) == x &&
		intMultiplication.op(intMultiplication.zero, x) == x
	}

	property("monoidLaws for booleanOr") = forAll { (x: Boolean, y: Boolean, z: Boolean) =>
		booleanOr.op(booleanOr.op(x, y), z) == booleanOr.op(x, booleanOr.op(y, z)) &&
		booleanOr.op(x, booleanOr.zero) == x &&
		booleanOr.op(booleanOr.zero, x) == x
	}

	property("monoidLaws for booleanAnd") = forAll { (x: Boolean, y: Boolean, z: Boolean) =>
		booleanAnd.op(booleanAnd.op(x, y), z) == booleanAnd.op(x, booleanAnd.op(y, z)) &&
		booleanAnd.op(x, booleanAnd.zero) == x &&
		booleanAnd.op(booleanAnd.zero, x) == x
	}

	property("monoidLaws for optionMonoid") = forAll { (x: Option[Int], y: Option[Int], z: Option[Int]) =>
		optionMonoid.op(optionMonoid.op(x, y), z) == optionMonoid.op(x, optionMonoid.op(y, z)) &&
		optionMonoid.op(x, optionMonoid.zero) == x &&
		optionMonoid.op(optionMonoid.zero, x) == x
	}

	property("monoidLaws for endoMonoid") = forAll { (x: Int => Int, y: Int => Int, z: Int => Int) =>
		endoMonoid.op(endoMonoid.op(x, y), z)(10) == endoMonoid.op(x, endoMonoid.op(y, z))(10) &&
		endoMonoid.op(x, endoMonoid.zero)(10) == x(10) &&
		endoMonoid.op(endoMonoid.zero, x)(10) == x(10)
	}

	// property("monoidLaws for listMonoid[A]") = forAll[A] { (x: List[A], y: List[A], z: List[A]) =>
	// 	listMonoid.op(listMonoid.op(x, y), z) == listMonoid.op(x, listMonoid.op(y, z)) &&
	// 	listMonoid.op(x, listMonoid.zero) == x &&
	// 	listMonoid.op(listMonoid.zero, x) == x
	// }
}
