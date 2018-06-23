package chapters

object Chapter4Monads {

  /**
    * Whereas a Functor allows us to sequence computations, a Monad (which is also Functor)
    * allows us to sequence computations whilst taking into account an intermediate complication.
    * E.g the flatmap method of Option takes intermediate Options into account
    *
    * The function passed to a flatmap specifies the application-specific part of the computation
    * and flatMap takes care of the complication
    */

  import scala.util.Try

  def foo(s: String): Option[Int] = Try(s.toInt).toOption

  foo("1").flatMap(a => foo("2").flatMap(b => foo("3").map(c => a + b + c)))
  for {
    a <- foo("1")
    b <- foo("2")
    c <- foo("3")
  } yield a + b + c //Some(6)

  for {
    a <- foo("1")
    b <- foo("?")
    c <- foo("3")

  } yield a + b + c //None


  /**
    * Simple definition of a Monad
    *
    * Laws
    * - Left identity - calling pure and transforming the result with func is the same as calling func
    *   pure(a).flatMap(func) == func(a)
    *
    * - Right identity - passing pure to flatMap is the same as doing nothing
    *    m.flatMap(pure) == m
    *
    *  - Associativity - flatMapping over two functions f and g is the same as flatMapping over f and
    *   then flatMapping over g
    *   m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
    *
    */
  trait Monad[F[_]] {
    def pure[A](value: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    // Exercise.  Define map
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  /**
    * Monads in Cats
    */


}
