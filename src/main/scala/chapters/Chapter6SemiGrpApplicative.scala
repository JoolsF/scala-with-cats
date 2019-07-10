package chapters

object Chapter6SemiGrpApplicative {

  import cats.Semigroupal
  import cats.instances.option._

  /**
    * Semigroup is type class that allows us to combine contexts
    */

  // Self-explanatory
  Semigroupal[Option].product(Some(1), Some(2)) //Some((1,2))
  Semigroupal[Option].product(Some(1), None) //None

  // Combining more than 2 contexts

  Semigroupal.tuple3(Option(1), Option(2), Option(3)) //Some((1,2,3))
  Semigroupal.map3(Option(1), Option(2), Option(3))((a, b, c) => a - b * c) //Some(-5)

  import cats.syntax.apply._
  //Apply syntax
  (Option("foo"), Option("bar"), Option("baz")).tupled // Some((foo,bar,baz))

  // Map n - takes implicit Functor and function of correct arity
  case class Person(firstName: String, surname: String, age: Int)

  (
    Option("Julian"),
    Option("Fenner"),
    Option(38)
  ).mapN(Person.apply) //Some(Person(Julian,Fenner,38))

  (
    Option("Julian"),
    Option("Fenner"),
    None
  ).mapN(Person.apply) // None

  /**
    * Non-intuitive behaviour of semigroup
    * Despite this we can create useful data types that have instances of
    * Semigroupal (and Applicative) but not Monad.
    */

  // Either
  // The following does not accumulate errors but fails fast liek a Monad
  // This is because Either is a Monad
  type ErrorOr[A] = Either[Vector[String], A]

  import cats.instances.either._ // for Semigroupal
  Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  ) //Left(Vector(Error 1))

  import cats.Monad

  // Exercise - 6.3.1.1
  // Implement product in terms of flatMap

  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for map

  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {

    for {
      xx <- x
      yy <- y
    } yield (xx, yy)
  }

  /**
    * Validated
    * This data type has an instance of semigroupal but not instance of Monad.  It is therefore
    * free to accumulate errors.
    */

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._

  type AllErrorsOr[A] = Validated[List[String], A]

  Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  ) // res1: AllErrorsOr[(Nothing, Nothing)] = Invalid(List(Error 1, Error 2))

  // The two subtypes Valid and Invalid broadly map to Right and Left

  val v: AllErrorsOr[Int] = Validated.valid(1)
  val e: AllErrorsOr[Int] = Validated.invalid(List("Error 1"))

  // Syntax can also be used

  import cats.syntax.validated._

  123.valid[List[String]] //cats.data.Validated[List[String],Int] = Valid(123)
  List("Error").invalid[Int] //cats.data.Validated[List[String],Int] = Invalid(List(Error))

  // Pure and raise error can also be used

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._ // for raiseError

  type ErrorsOr[A] = Validated[List[String], A]

  123.pure[ErrorsOr] //ErrorsOr[Int] = Valid(123)
  List("error").raiseError[ErrorsOr, Int] //ErrorsOr[Int] = Invalid(List(error))

  // They can also be created from Exception as well as instances of Try, Either and Option

  Validated.catchOnly[NumberFormatException]("foo".toInt) // cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")
  Validated.fromTry(scala.util.Try("foo".toInt)) //cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")
  Validated.fromEither(Left(List("error"))) // cats.data.Validated[List[String],Nothing] = Invalid(List(error))

//  (
//    "Error 1".invalid[Int],
//    "Error 2".invalid[Int]
//  ).tupled

  /**
    * Methods of validated
    */

  123.valid.map(_ * 100) //cats.data.Validated[Nothing,Int] = Valid(12300)
  123.valid[Int].bimap(_ + "!", _ * 100) // cats.data.Validated[String,Int] = Valid(12300)
  "?".invalid[String].bimap(_ + "!", _ * 100) // cats.data.Validated[String,String] = Invalid(?!)

  // Flat mapping - we can't do this as its not a monad but we can covert back and forth with either
  import cats.syntax.either._ // for toValidated

  "Badness".invalid[Int]
  // res21: cats.data.Validated[String,Int] = Invalid(Badness)

  "Badness".invalid[Int].toEither
  // res22: Either[String,Int] = Left(Badness)

  "Badness".invalid[Int].toEither.toValidated
  // res23: cats.data.Validated[String,Int] = Invalid(Badness)

  // Temporarily converting to either
  41.valid[String].withEither(_.flatMap(n => Right(n + 1))) //cats.data.Validated[String,Int] = Valid(42)

  {
    -123
  }.valid[String].ensure("Negative")(_ > 0) //Invalid(Negative)

  "fail".invalid[Int].getOrElse(2) // 2
  1.valid[String].getOrElse(2) // 1

  /**
    * Exercise 6.4.4 - Form validation
    */
  case class User(name: String, age: Int)

  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def createUser(html: Map[String, String]): FailSlow[User] =
    (
      readName(html).toValidated,
      readage(html).toValidated
    ).mapN(User.apply)


  def readName(html: Map[String, String]): FailFast[String] =
    for {
      name <- getValue("name", html)
      res <- nonBlank(name)
    } yield res


  def readage(html: Map[String, String]): FailFast[Int] =
    for {
      ageStr <- getValue("age", html)
      age <- parseInt(ageStr)
      res <- ageMustNotBeNegative(age)
    } yield res


  //Predicates
  def ageMustNotBeNegative(age: Int): FailFast[Int] =
    Either.cond(age >= 0, age, List(s"Age $age must not be negative"))

  def foo(age: Int): FailFast[Int] =
    Either.cond(age != -1, age, List(s"Age $age must not be -1"))


  def getValue(key: String, html: Map[String, String]): FailFast[String] =
    html.get(key).toRight(List(s"Key $key does not exist"))

  def parseInt(str: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](str.toInt)
      .leftMap(throwable => List(s"$str must be an integer"))


  def nonBlank(str: String): FailFast[String] =
    Right(str)
      .ensure(List(s"$str cannot be blank"))(_.length > 0)

}
