package chapters

import cats.data.{Writer, WriterT}

import scala.concurrent.{Await, Future}
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
  import cats.instances.list._
  import cats.instances.option._

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
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for flatMap
  //  import scala.language.higherKinds


  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      aVal <- a
      bVal <- b
    } yield aVal * aVal + bVal * bVal

  }

  import cats.instances.list._
  import cats.instances.option._ // for Monad”

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


  import cats.Eval

  /**
    * Eval Monad
    * Think of val, lazy val and def
    * val / now        - both eagerly evaluated and the result is memoized i.e it can be recalled without recomputing
    * lazy val / later - both lazily evaluated when called and the result is memoized
    * def / always     - both lazily evaluated and the result is not memoized
    **/

  val now = Eval.now(math.random() + 1000) // now: cats.Eval[Double] = Now(1000.3050385215279)

  val later = Eval.later(math.random() + 1000) // later: cats.Eval[Double] = cats.Later@28375e21

  val always = Eval.always(math.random + 3000) // always: cats.Eval[Double] = cats.Always@308287f9


  now.value
  now.value

  later.value
  later.value

  always.value
  always.value

  /*
   * Eval has a memoize method that allows us to memoize a chain of computations. The result of the chain up to the
   * call to memoize is cached, whereas calculations after the call retain their original semantics:
   */

  val saying = Eval.
    always {
      println("Step 1");
      "The cat"
    }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }
  // saying: cats.Eval[String] = cats.Eval$$anon$8@7a0389b5
  saying.value // first access
  // Step 1
  // Step 2
  // Step 3
  // res18: String = The cat sat on the mat
  saying.value // second access
  // Step 3
  // res19: String = The cat sat on the mat

  /**
    * Trampolining and Eval.defer
    * One useful property of Eval is that its map and flatMap methods are trampolined.
    * This means we can nest calls to map and flatMap arbitrarily without consuming stack frames.
    * Eval is therefore stacksafe
    */

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  factorial(50000).value

  /**
    * Exercise 4.6.5
    * Safer folding using Eval
    *
    * Make the naive implementation of foldRight stacksafe
    */
  def foldRightNaive[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRightNaive(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def myFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }
  }.value


  /**
    * The Writer Monad
    * cats.data.Writer is a monad that lets us carry a log along with a computation. We can use it to record messages,
    * errors, or additional data about a computation, and extract the log alongside the final result.
    * A Writer[W, A] carries two values: a log of type W and a result of type A
    */

  val w1: WriterT[cats.Id, Vector[String], Int] = //Notice that the type is WriterT.  Writer is implemented in terms of WriterT
    Writer(
      Vector(
        "I am message one",
        "I am message two"
      ),
      42
    )

  //Syntax can be used with a way by only specifying the log or result
  //To do this we must have a Monoid[W] in scope so Cats knows how to produce an empty log:

  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure

  type Logged[A] = Writer[Vector[String], A]

  123.pure[Logged]

  //If we have a log and no result we can create a Writer[Unit] using the tell syntax from cats.syntax.writer:

  import cats.syntax.writer._

  Vector("Log message 1", "Log message 2").tell


  //If we have both...

  123.writer(Vector("Log message 1", "Log message 2"))

  //To get the result...

  w1.value //cats.Id[Int] = 42

  //To get the result
  w1.written //cats.Id[Vector[String]] = Vector(I am message one, I am message two)

  //To get both

  w1.run //cats.Id[(Vector[String], Int)] = (Vector(I am message one, I am message two),42)


  /**
    * Composing and transforming writers
    * The log is preserved when we map and flatmap over it
    * It’s good practi􏰀ce to use a log type that has an efficient append and concatenate opera􏰀ons, such as a Vector:
    */

  //cats.data.WriterT[cats.Id,Vector[String],Int] =
  // WriterT((Vector(123 result, 456 result, another message),579))
  val w2 = for {
    a <- 123.pure[Logged]
    _ <- Vector("123 result").tell
    b <- 456.writer(Vector("456 result"))
    _ <- Vector("another message").tell
  } yield a + b
  w2.run
  // cats.Id[(Vector[String], Int)] = (Vector(123 result, 456 result, another message),579)


  //We can transform the logs
  w2.mapWritten(_.map(_.toUpperCase))
  // cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] = WriterT((Vector(123 RESULT, 456 RESULT, ANOTHER MESSAGE),579))

  //Or transform both
  w2.bimap(
    _.map(_.toUpperCase),
    _ * 2
  )
  // cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] = WriterT((Vector(123 RESULT, 456 RESULT, ANOTHER MESSAGE),1158))

  w2.mapBoth { (log, result) =>
    (log.map(_ + "1"), result * 3)
  }
  // cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] = WriterT((Vector(123 result!, 456 result!, another message!),1737))

  /**
    * 4.7.3 Exercise: Show Your Working
    * Transform factorial to run in parallel in such a way that, instead of interleaving the log,
    * each log is seperate
    */

  import scala.concurrent.duration._

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val Vector((logA, ansA), (logB, ansB)) =
    Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(5).run)
    )), 5.seconds)


  /**
    * The Reader Monad
    * cats.data.Reader is a monad that allows us to sequence opera􏰀ons that de- pend on some input. Instances of Reader
    * wrap up func􏰀ons of one argument, providing us with useful methods for composing them.
    * One common use for Readers is dependency injecti􏰀on. If we have a number of opera􏰀ons that all depend on some
    * external configurati􏰀on, we can chain them together using a Reader to produce one large opera􏰀on that accepts
    * the configurati􏰀on as a parameter and runs our program in the order specified.
    *
    */


  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  // Creating a Reader through the apply function
  val catName: Reader[Cat, String] = Reader(cat => cat.name)


  /**
    * 4.8.3 Exercise: Hacking on Readers
    * The classic use of Readers is to build programs that accept a configuration as a
    * parameter. Let’s ground this with a complete example of a simple login system.
    * Our configuration will consist of two databases: a list of valid users and a
    * list of their passwords:
    */

  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String]
               )


  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(_.usernames.get(userId))

  def checkPassword(username: String,
                    password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  import cats.syntax.applicative._ // for pure

  def checkLogin(userId: Int,
                 password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUsername(userId)
      passwordOk <- maybeUsername match {
        case Some(u) => checkPassword(u, password)
        case None => false.pure[DbReader]
      }
    } yield passwordOk


  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  checkLogin(1, "zerocool").run(db)
  // res10: cats.Id[Boolean] = true
  checkLogin(4, "davinci").run(db)
  // res11: cats.Id[Boolean] = false
}

