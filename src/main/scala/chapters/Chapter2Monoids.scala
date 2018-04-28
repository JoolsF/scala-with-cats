package chapters

/**
  * Monoids and semigroups
  */
object Ch2MonoidBasics {


  /**
    * A monoid is a type with a combine operation of type (A, A) => A
    * and an identity operation of type A
    *
    * Note that a semgigroup is just the combine part of a Monoid.
    * Here we have split the type up into traits as some types
    * for which we cannot define an identity element.
    */
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  /**
    * It follows the following laws.
    * It must be associative and follow the identity law
    */
  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }

  /**
    *
    * 2.3 the truth about monoids
    * Consider Boolean. How many monoids can you define for this type? For each monoid,
    * define the combine and empty opera ons and convince yourself that the monoid laws hold.
    * Use the following definitions as a staring point
    */
  object BooleanMonoidInstances {

    implicit def and = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y

      override def empty: Boolean = true
    }

    implicit def or = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y

      override def empty: Boolean = false
    }

    implicit def xor = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x ^ y

      override def empty: Boolean = false
    }
  }

  object SetMonoidInstances {

    implicit def union[A] = new Monoid[Set[A]] {
      override def combine(x: Set[A], y: Set[A]) = x.union(y)

      override def empty: Set[A] = Set.empty[A]
    }

    // this is not a Monoid since it doesn't obey the identity law
    implicit def intersection = new Semigroup[Set[Int]] {
      override def combine(x: Set[Int], y: Set[Int]) = x.intersect(y)
    }
  }


}


object Ch2MonoidCats {


  /**
    * Monoids in cats
    */

  import cats.Monoid
  import cats.instances.string._

  Monoid[String].combine("hello ", "world ") // hello world
  Monoid[String].empty // ""

  // if we dont need empty we can write
  import cats.Semigroup

  Semigroup[String].combine("hello ", "world ")

  //   we can assemble a Monoid[Option[String]] as follows
  import cats.instances.option._

  Monoid[Option[String]].combine(None, Some("hello world")) //Some(hello world)

  // syntax

  import cats.syntax.semigroup._ // for |+|
  import cats.instances.int._

  1 |+| 2 |+| 3 |+| Monoid[Int].empty // 6

  import cats.instances.map._ // for Monoid
  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)
  map1 |+| map2
  // res3: Map[String,Int] = Map(b -> 5, d -> 4, a -> 1)

  /**
    * 2.5.4 Exercise: Adding All The Things
    * Implement def add(items: List[Int]): Int.
    */
  def add(items: List[Int]): Int =
    items.foldLeft(Monoid[Int].empty) { (acc, i) =>
      Monoid.combine(acc, i)
    }

  /**
    * Implement add List[Option[Int]]. Change add so this is possible
    */
  def addO(items: List[Option[Int]]): Option[Int] =
    items.foldLeft(Monoid[Option[Int]].empty) { (acc, i) =>
      Monoid.combine(acc, i)
    }

  /**
    * Generalise the above and improve the code using syntax and implicits
    */
  def addS[A](items: List[A])(implicit m: Monoid[A]): A =
    items.foldLeft(Monoid[A].empty)((acc, e) => acc |+| e)

  /**
    * Now we want to add up Orders
    */

  case class Order(totalCost: Double, quantity: Double)

  implicit def orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    override def empty: Order = Order(0,0)
  }

  addS(List(Order(1.2,2), Order(4.3,5))) //Order(5.5, 7.0)

}
