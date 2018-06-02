package chapters

/**
  * Functors
  */

object Ch3Exercises {

  import cats.Functor
  import cats.syntax.functor._

  // Write a Functor for the following binary tree data type.
  // Verify that the code works as expected on instances of Branch and Leaf:
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunc = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
      case Leaf(v) =>
        Leaf(f(v))
    }
  }
  val tree1 = Branch(
    Leaf(1),
    Leaf(2)
  )

  // tree1.map(_ * 2)
  // this fails to compile with below as we only have Functor for Tree not Branch
  // need to add smart constructors

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  Tree.leaf(100).map(_ * 2) // Leaf(200)
  Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2) //  Branch(Leaf(20),Leaf(40))


}


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
   Higher Kinded Types

   https://typelevel.org/blog/2016/08/21/hkts-moving-forward.html
   https://stackoverflow.com/questions/6246719/what-is-a-higher-kinded-type-in-scala
   http://adriaanm.github.io/research/2010/10/06/new-in-scala-2.8-type-constructor-inference/
   https://medium.com/bigpanda-engineering/understanding-f-in-scala-4bec5996761f

    List    // type constructor, takes one parameter (WONT COMPILE)
    List[A] // type, produced using a type parameter.  Also a 'first order type'.

    analogous to

     math.abs    // function, takes one parameter
     math.abs(x) // value, produced using a value parameter


    A type constructor is a function that takes a type and returns a type
    List[_] is T => List[T]
    It could be thought of as a type level function that works with types rather than values

    1, "a", List(1,2,3) are values

    Int, String, List[Int] are proper types

    List[_], Option[_] are type constructors, takes a type and constructs a new type.
    Can be generalized with this syntax F[_]

    G[F[_]] is a type constructor that takes another type constructor like, Functor[F[_]],
    can be thought of as a higher order function at the type level

  */

  /**
    * Functors in Cats
    * Cats definition of Functor allows us to create instances for any single-parameter type constructor,
    * such as List, Option, Future, or a type alias such as MyFunc.
    */

  // Look like this.  Note this user higher kinded type syntax
  trait MyFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    // Cats functor basic example

    import cats.Functor
    import cats.instances.list._ // for Functor
    import cats.instances.option._ // for Functor

    val list1 = List(1, 2, 3)

    Functor[List].map(list1)(_ * 3) //List(3, 6, 9)

    val option1: Option[Double] = Some(Math.PI)

    Functor[Option].map(option1)(_.toInt) //Some(3)


    // Functor also has a lift function
    // (A => B) => (F[A] => F[B])


    val func1: Int => Int = (x: Int) => x + 1

    val liftedFunc1: Option[Int] => Option[Int] = Functor[Option].lift(func1)

    liftedFunc1(Some(1)) //Some(2)

    // The main method provided by the syntax for Functor is map.
    // Can be thought of as a way of appending a transformation to a chain
    import cats.syntax.functor._ // for map

    val f1 = (x: Int) => x * 2
    val f2 = (x: Int) => (x, x * x)
    val f3 = (x: (Int, Int)) => (s"${x._1.toString}!", x._2.toDouble)

    val f1to3 = f1.map(f2).map(f3)
    f1to3(1) //(String, Double) = (2!,4.0)


    // applies equation to any functor no matter what the context is
    def doMath[F[_]](start: F[Int])
                    (implicit functor: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)


    doMath(Option(10))
    doMath(List(10, 11))

    //Instances for custom types

    case class Foo[A](a: A) {
      def transform[B](f: A => B): Foo[B] = Foo(f(a))
    }

    implicit val fooF = new Functor[Foo] {
      def map[A, B](value: Foo[A])(func: A => B): Foo[B] =
        value.transform(func)
    }


    Foo(1).map(_ + 2) //Foo 3
  }


}



