package chapters


/**
  * Type Classes
  */
object Ch1Exercises {

  /** **************************
    * Ex 1.3
    * **************************/
  final case class Cat(name: String, age: Int, color: String)

  trait Printable[A] {

    def format(value: A): String

  }

  object PrintableInstances {
    implicit val printableString: Printable[String] = new Printable[String] {
      override def format(value: String) = value
    }

    implicit val printableInt: Printable[Int] = new Printable[Int] {
      override def format(value: Int) = value.toString
    }

    implicit val printableCat: Printable[Cat] = new Printable[Cat] {
      override def format(c: Cat) = s"${c.name} is a ${c.age} year-old ${c.color} cat"
    }

  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))

  }

  object PrintableSyntax {

    implicit class PrintableOps[A](v: A) {
      def format(implicit p: Printable[A]) = p.format(v)

      def print(implicit p: Printable[A]) = println(format)

    }

  }

  import PrintableInstances._
  import Printable._

  val c = Cat("Julian", 37, "Black")
  print(c) // Julian is a 37 year-old Black cat

  import PrintableSyntax._

  c.print // Julian is a 37 year-old Black cat

  /**
    *  1.5.4
    * Re-implement the Cat application using Show instead of Printable.
    */

  import cats.syntax.show._
  import cats.Show

  implicit def showCat: Show[Cat] = Show.show { c =>
    s"${c.name} is a ${c.age} year-old ${c.color} cat"
  }

  c.show

  /**
    * 1.5.5
    * Implement Eq[Cat]
    */

  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.int._
  import cats.instances.string._

  implicit val catEq: Eq[Cat] = Eq.instance { (cat1, cat2) =>
    (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
  }

}

object Ch1Notes {
  /**
    * Show type class
    */
  //import type classes
  import cats.Show

  //each import provides instances of all Cats’ type classes for a specific parameter type:
  import cats.instances.int._

  val showInt = Show.apply[Int]
  showInt.show(1)

  // cats provides different syntax imports for each type class
  import cats.syntax.show._

  1.show

  // implementing custom instances
  import java.time.LocalDateTime

  implicit def dateTimeShow = new Show[LocalDateTime] {
    def show(t: LocalDateTime): String =
      s"Hour: ${t.getHour} Min: ${t.getMinute} Sec: ${t.getSecond}"
  }

  val a = java.time.LocalDateTime.now()
  a.show

  // show also contains convenience methods
  case class Person(firstName: String, surname: String, age: Int)

  val p = Person("Julian", "Fenner", 37)
  val pToString: Person => String = p => s"First name: ${p.firstName} Surname: ${p.surname} Age ${p.age}"

  implicit val showPerson: Show[Person] = Show.show(pToString)


  /**
    * Eq - Type safe equals
    * trait Eq[A] {
    * def eqv(a: A, b: A): Boolean
    * // other concrete methods based on eqv...
    * }
    *
    * === compares two objects for equality
    *  =!= compares two objects for inequality.
    */

  import cats.Eq

  val eqInt = Eq[Int]
  eqInt.eqv(1, 2)

  //  We can also import the interface syntax in cats.syntax.eq to use the === and =!= methods:
  import cats.syntax.eq._

  123.===(123)

  // Comparing custom type
  implicit val personCompare: Eq[Person] = Eq.instance { (p1, p2) =>
    p1.firstName == p2.firstName &&
      p1.surname == p2.surname &&
      p1.age == p2.age
  }

  p =!= p // false

}
