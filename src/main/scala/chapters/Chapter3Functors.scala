package chapters

/**
  * Functors
  */
object Chapter3 {
  /**
    * Informally its anything with a map method.
    * Should be thought of as transforming the collection, not iterating across it.
    * The values change but structure remains the same.  This allows pipelining of maps for example.
    * Its a way of sequencing computations with any complications taken care of by the type e.g
    *
    * Either - value or an error
    * Option - value or 'nothing'
    * List - zero or more values
    *
    * It encapsulates sequencing computations.
    * Its a type F[A] with an operation map with type (A => B) => F[B]
    *
    * LAWS
    * Identity: calling map with the identity function is the same as doing nothing:
    * fa.map(a => a) == fa
    * Composition: mapping with two functions f and g is the same as mapping with f and then mapping with g:
    * fa.map(g(f(_))) == fa.map(f).map(g)
    */

  /**
    * Single arg functions are a type of functor
    * Function composition is sequencing.
    * By mapping we add another operation to the chain, we don't run anything until we pass an argument
    */
  val a: String => Int = s => s.toInt
  val b: Int => List[Double] = i => List(i, i, i)

  // Mapping over a function is function composition
  import cats.instances.function._
  import cats.syntax.functor._ // for map

  // composition with map
  (a map b) ("3") // List(3.0, 3.0, 3.0)

  // composition with andThen

  (a andThen b) ("3") // List(3.0, 3.0, 3.0)


  /**
    * Functors in Cats
    */

  // Look like this.  Note this user higher kinded type syntax
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

}

object Chapter4HigherKindedTypes {
  // https://typelevel.org/blog/2016/08/21/hkts-moving-forward.html
  // https://stackoverflow.com/questions/6246719/what-is-a-higher-kinded-type-in-scala
  // http://adriaanm.github.io/research/2010/10/06/new-in-scala-2.8-type-constructor-inference/
}
