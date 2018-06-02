package chapters

object Chapter3bVariance {

  /**
    * Contravariant and Invariant Functors
    *
    * Functors map is a of appending a transformation to a chain.
    * A contravariant functor represents prepending operations.
    * Invariant functor represents a bidirectional chain of operations.
    */

  final case class Box[A](value: A)

  trait Printable[A] {
    self => // used as 'this' ambivalent in the context of the new Printable[B] class below
    def format(value: A): String

    def contramap[B](func: B => A) = new Printable[B] {
      println("3 instantiate Printable[Box[Boolean]] with contramap")

      override def format(value: B): String = {
        println("5 apply format Printable[Box[Boolean]]")
        self.format(func(value))
      }
    }
  }

  implicit def booleanPrintable = new Printable[Boolean] {
    println("1 instantiate booleanPrintable")

    def format(value: Boolean) = {
      println("6 apply Printable[Boolean]")
      if (value) "yes" else "no"
    }
  }


  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = {
    println("2 box printable args resolved")
    // booleanPrintable
    p.contramap[Box[A]]((b: Box[A]) => b.value)
  }


  def format[A](value: A)(implicit p: Printable[A]): String = {
    println("4 params resolved, inside format method")
    // boxPrintable Printable[Box[Boolean]]
    p.format(value)
  }

  format(Box(true))

  //// boxPrintable above could be instantiated as
  //implicit def boxPrintable[A](implicit p: Printable[A]) = new Printable[Box[A]] {
  //  println("2 box printable args resolved")
  //  def format(box: Box[A]): String =
  //    p.format(box.value)
  //}


  /**
    * Invariant functors and the imap method
    *
    * Invariant functors implement a method called imap that is informally equivalent to a combination of map
    * and contramap. If map generates new type class instances by appending a func on to a chain, and
    * contramap generates them by prepending an opera on to a chain, imap generates them via a
    * pair of bidirectional transformations.
    */
  trait Codec[A] {
    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        def encode(value: B): String =
          self.encode(enc(value))

        def decode(value: String): B =
          dec(self.decode(value))
      }
    }
  }

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap(Box(_), _.value)


  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)


  encode(123.4) // res0: String = 123.4

  decode[Double]("123.4") // res1: Double = 123.4

  encode(Box(123.4)) // res2: String = 123.4

  decode[Box[Double]]("123.4") // res3: Box[Double] = Box(123.4)


  /**
    * Contravariant and Invariant in Cats
    *
    */

  // Simplified version of cats implementation
  //  trait Contravariant[F[_]] {
  //    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  //  }
  //  trait Invariant[F[_]] {
  //    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  //  }
  //
  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]

  val showSymbol: Show[Symbol] =
    Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

  showSymbol.show('dave) // res2: String = 'dave

  // we can also use Syntax as usual
  import cats.syntax.contravariant._ // for contramap

  showString.contramap[Symbol](_.name).show('dave)

}
