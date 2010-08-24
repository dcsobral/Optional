// Scala version 2.8.0.final
// http://scalacheck.googlecode.com/files/scalacheck_2.8.0-1.8-SNAPSHOT.jar


/*
  Below are 15 exercises. The task is to emulate the scala.Option API
  without using Some/None subtypes, but instead using a fold (called a
  catamorphism).

  A couple of functions are already done (map, get)
  to be used as an example. ScalaCheck tests are given below to
  verify the work. The desired result is to have all tests passing.

  The 15th exercise is not available in the existing Scala API so
  instructions are given in the comments.

  Revision History
  ================

  23/08/2010
  Initial revision

  ----------------

*/


trait Optional[A] {
  // single abstract method
  def fold[X](some: A => X, none: => X): X

  import Optional._

  // Done for you.
  def map[B](f: A => B): Optional[B] =
    fold(f andThen some, none[B])

  // Done for you.
  // WARNING: undefined for None
  def get: A =
    fold(a => a, error("None.get"))

  // Exercise 1
  def flatMap[B](f: A => Optional[B]): Optional[B] =
    fold(f, none[B])

  // Exercise 2
  // Rewrite map but use flatMap, not fold.
  def mapAgain[B](f: A => B): Optional[B] =
    flatMap(f andThen some)

  // Exercise 3
  def getOrElse(e: => A): A =
    fold(a => a, e)

  // Exercise 4
  def filter(p: A => Boolean): Optional[A] =
    flatMap(a => if (p(a)) some(a) else none[A])

  // Exercise 5
  def exists(p: A => Boolean): Boolean =
    fold(p, false)

  // Exercise 6
  def forall(p: A => Boolean): Boolean =
    fold(p, true)

  // Exercise 7
  def foreach(f: A => Unit): Unit =
    fold(f, ())

  // Exercise 8
  def isDefined: Boolean =
    fold(_ => true, false)

  // Exercise 9
  def isEmpty: Boolean =
    fold(_ => false, true)

  // Exercise 10
  def orElse(o: Optional[A]): Optional[A] =
    fold(some, o)

  // Exercise 11
  def toLeft[X](right: => X): Either[A, X] =
    fold(Left(_), Right(right))

  // Exercise 12
  def toRight[X](left: => X): Either[X, A] =
    fold(Right(_), Left(left))

  // Exercise 13
  def toList: List[A] =
    fold(List(_), Nil)

  // Exercise 14
  def iterator: Iterator[A] =
    fold(Iterator(_), Iterator())

  // Exercise 15 The Clincher!
  // Return a none value if either this or the argument is none.
  // Otherwise apply the function to the argument in some.
  // Don't be afraid to use functions you have written.
  // Better style, more points!
  def applic[B](f: Optional[A => B]): Optional[B] =
    f flatMap map

  // Utility
  def toOption: Option[A] = fold(Some(_), None)
}

object Optional {
  // Done for you
  def none[A]: Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = none
  }

  // Done for you
  def some[A](a: A): Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = some(a)
  }

  // Utility
  def fromOption[A](o: Option[A]): Optional[A] = o match {
    case None    => none
    case Some(a) => some(a)
  }
}
