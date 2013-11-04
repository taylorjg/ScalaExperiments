import java.util.regex._
import fpinscala.datastructures._
import fpinscala.datastructures.Option._

object Exercise6 {

	private def pattern(s: String): Option[Pattern] =
		try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}	

	/*
 	 * private def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
 	 *	sequence(a map f)
 	 */

 	private def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
 		a.foldRight[Option[List[B]]](Some(Nil))((a, bso) => map2(f(a), bso)(_ :: _))

	private def parsePatterns(a: List[String]): Option[List[Pattern]] =
		traverse(a)(pattern(_))

	private def showResult1(a: List[String]): Unit =
		println("parsePatterns(%s): %s".format(a, parsePatterns(a)))

	private def sequenceInTermsOfTraverse[A](as: List[Option[A]]): Option[List[A]] =
		traverse(as)(identity)

	private def showResult2[A](as: List[Option[A]]): Unit =
		println("sequenceInTermsOfTraverse(%s): %s".format(as, sequenceInTermsOfTraverse(as)))

	def main(args: Array[String]): Unit = {

		showResult1(List("[a]", "[b]"))
		showResult1(List("[a", "[b]"))
		showResult1(List("[a]", "[b"))

		showResult2(List(Some(1), Some(2), Some(3)))
		showResult2(List(Some(1.0), Some(2.0), Some(3.0)))
		showResult2(List(Some("A"), Some("B"), Some("C")))

		showResult2(Nil: List[Option[Int]])
		showResult2(Nil: List[Option[Double]])
		showResult2(Nil: List[Option[String]])

		showResult2(List(None, Some(2), Some(3)))
		showResult2(List(Some(1), None, Some(3)))
		showResult2(List(Some(1), Some(2), None))
		showResult2(List(None, None, None))
	}
}
