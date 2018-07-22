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

  foo("1")
    .flatMap(a => foo("2")
      .flatMap(b => foo("3")
        .map(c => a + b + c)))
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
    * pure(a).flatMap(func) == func(a)
    *
    * - Right identity - passing pure to flatMap is the same as doing nothing
    *    m.flatMap(pure) == m
    *
    *  - Associativity - flatMapping over two functions f and g is the same as flatMapping over f and
    * then flatMapping over g
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

  trait MonadWithLaws[F[_]] extends Monad[F[_]] {

    def flatMap[A, B](value: F[A])(func: A => F[B]): Boolean

  }

  /**
    * Monads in Cats
    */
  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._

  val opt1: Option[Int] = Monad[Option].pure(3) // Some(3)

  val opt2: Option[Int] = Some(1).flatMap(i => Some(i * 2)) // Some(6)

  val opt3 = Monad[Option].map(opt2)(_ * 3) //Some(18)

  val list1 = Monad[List].pure(3) //List(3)

  val list2 = Monad[List].flatMap(List(1, 2, 3))(i => List(i, i * 9)) //List(1, 9, 2, 18, 3, 27)

  val list3 = Monad[List].flatMap(list2)(x => List(x * 10)) //List(10, 90, 20, 180, 30, 270)

  /**
    *
    * Default instances
    */


  //import cats.instances.option._ // for Monad

  Monad[Option].flatMap(Option(1))(a => Option(a*2))


}
