package chapters

import cats.Monad

import scala.util.Try

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

  //  trait MonadWithLaws[F[_]] extends Monad[F[_]] {
  //
  //    def flatMap[A, B](value: F[A])(func: A => F[B]): Boolean
  //
  //  }

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

}

object Chapter4Monad_2 {

  /**
    * Syntax
    *
    * Example abstracting over different monads
    */

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  import scala.language.higherKinds


  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      aVal <- a
      bVal <- b
    } yield aVal * aVal + bVal * bVal

  }

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad”

  sumSquare(List(1, 2, 3), List(4, 5)) // res0: List[Int] = List(17, 26, 20, 29, 25, 34)
  sumSquare(Option(2), Option(9)) // res1: Option[Int] = Some(85)


  /**
    * Identity Monad
    *
    * Allows similar to above but can use Monadic and non-Monadic parameters
    * This is extremely powerful as it means that we can run code asynchronously in production
    * and synchronously in tests
    */

  import cats.Id

  sumSquare(3: Id[Int], 4: Id[Int]) // 25

  /*
   * type Id[A] = A
   *
   * Id is a type alias that turns at atomic type into a single-parameter type constructor
   *
   */

  val a = Monad[Id].pure(3)

  /**
    * Implement pure map and flatMap for Id
    */

  type MyId[A] = A

  def pure[A](value: A): MyId[A] = value

  def map[A, B](value: MyId[A])(f: A => B): MyId[B] = f(value)

  def flatMap[A, B](value: MyId[A])(f: A => MyId[B]): MyId[B] = f(value)

  /*
   * The purpose of a Monad is to allow us to sequence operations ignoring a complication
   * However, in this of Id / MyId there is no intermediate complication, these are atomic types.
   * MyId[A] is simply an alias of A!
   * Pure returns the argument.
   * Map and flatMap are identical since A => B === A => Id[B]
   */

  /**
    * Either
    */

  import cats.syntax.either._

  4.asRight[String]

  case class Error(e: String)

  /*
    * These “smart constructors” have advantages over Left.apply and Right.apply
    * because they return results of type Either instead of Left and Right.
    * This helps avoid type inference bugs caused by over-narrowing
    */
  def sumPositive(l: List[Int]): Either[Error, Int] = {
    l.foldLeft(0.asRight[Error]) { (acc: Either[Error, Int], i: Int) =>
      if (i > 0) {
        acc.map(_ + i)
      } else {
        Left(Error(s"negative value in list $i"))
      }
    }
  }


  sumPositive(List(1, 2, 3, -1, 4)) // Left(Error(negative value in list -1))
  sumPositive(List(1, 2, 3, 4)) // Right(10)

  /*
    * Other useful Either extension methods
    */
  Either.catchOnly[NumberFormatException]("foo".toInt) // res3: Either[NumberFormatException,Int] = Left(java.lang.NumberFormatException: For input string: "foo")

  Either.catchNonFatal(sys.error("uh oh")) // res4: Either[Throwable,Nothing] = Left(java.lang.RuntimeException: uh oh)
  Either.fromTry(scala.util.Try("foo".toInt)) // res5: Either[Throwable,Int] = Left(java.lang.NumberFormatException: For input string: "foo")


  /*
   * Transforming Eithers
   */


  // p 373 Either from a try with a left map to handle error case

  def squareString(s: => String): Either[String, Int] = {
    Either.fromTry(
      Try {
        val i = s.toInt
        i * i
      }
    ).leftMap {
      case _: NumberFormatException => "Boom - number format exception!"
      case _: NullPointerException => "Bang - null pointer exception!"
    }
  }


  squareString("1") // res0: Either[String,Int] = Right(1)
  squareString("s") // res1: Either[String,Int] = Left(Boom - number format exception!)
  squareString(throw new NullPointerException) //res2: Either[String,Int] = Left(Bang - null pointer exception!)

  // Ensuring -  must satisfy a predicate

  def squareString(s: String, p: Int => Boolean): Either[String, Int] = {
    Either.fromTry( // Either from Try
      Try {
        val i = s.toInt
        i * i
      }
    ).leftMap { case _: NumberFormatException => "Boom - number format exception!" } // Left map
  }.ensure("result must not equal 9")(p) // Predicate to check with result satisfies a predicate

  val p: Int => Boolean = _ != 9

  squareString("2", p) // Right(4)
  squareString("2!", p) // Left(Boom - number format exception!!)
  squareString("3", p) // Left(must not equal 9)

  // Bimap
  def cubeString(s: String): Either[Error, String] = {
    Either.fromTry(
      Try {
        val i = s.toInt
        i * i * i
      }
    ).bimap(
      e => Error(e.getMessage),
      s => s"$s!!!"
    )
  }

  cubeString("4") // Right(64!!!)
  cubeString("foo") // Left(Error(For input string: "foo"))






}

