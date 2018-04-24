package chapters

/**
  * Monoids and semigroups
  */
object ChapterTwoExecises {

  /**
    *
    * 2.3 the truth about monoids
    * Consider Boolean. How many monoids can you define for this type? For each monoid,
    * define the combine and empty opera ons and convince yourself that the monoid laws hold.
    * Use the following definitions as a staring point
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


  object BooleanMonoidInstances {

    def and = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x && y

      override def empty: Boolean = true
    }

    def or = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x || y

      override def empty: Boolean = false
    }

    def xor = new Monoid[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x ^ y

      override def empty: Boolean = ???
    }


  }

}
