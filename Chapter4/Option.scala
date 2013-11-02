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

		def getOrElse[B >: A](default: => B): B = {
			this match {
				case Some(v) => v
				case _ => default
			}
		}
	}

	case class Some[+A](get: A) extends Option[A]
	case object None extends Option[Nothing]
}
