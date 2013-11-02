package fpinscala.datastructures {

	sealed trait Option[+A] {

		def map[B](f: A => B): Option[B] =
			this match {
				case Some(v) => Some(f(v))
				case _ => None
			}

		def flatMap[B](f: A => Option[B]): Option[B] =
			this match {
				case Some(v) => f(v)
				case _ => None
			}

		def getOrElse[B >: A](default: => B): B =
			this match {
				case Some(v) => v
				case _ => default
			}

		def orElse[B >: A](ob: => Option[B]): Option[B] =
			this match {
				case Some(_) => this
				case _ => ob
			}

		def filter(f: A => Boolean): Option[A] =
			this match {
				case Some(v) if f(v) => this
				case _ => None
			}
	}

	case class Some[+A](get: A) extends Option[A]
	case object None extends Option[Nothing]
}
