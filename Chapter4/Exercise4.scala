import java.util.regex._
import fpinscala.datastructures._
import fpinscala.datastructures.Option._

object Exercise4 {

	private def pattern(s: String): Option[Pattern] =
		try {
			Some(Pattern.compile(s))
		} catch {
			case e: PatternSyntaxException => None
		}	

	private def mkMatcher(pat: String): Option[String => Boolean] =
		for {
			p <- pattern(pat)
		} yield ((s: String) => p.matcher(s).matches)

	private def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
  		map2(mkMatcher(pat1), mkMatcher(pat2))(_(s) && _(s))

	private def showResult(pat1: String, pat2: String, s: String): Unit =
		println("bothMatch(\"%s\", \"%s\", \"%s\"): %s".format(pat1, pat2, s, bothMatch(pat1, pat2, s)))

	def main(args: Array[String]): Unit = {
		showResult("[a]", "[a]", "a")
		showResult("[a]", "[a]", "b")
		showResult("[a", "[a]", "a")
		showResult("[a]", "[a", "a")
		showResult("[a", "[a", "a")
	}
}
