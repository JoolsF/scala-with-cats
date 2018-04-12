package chapters

/**
  * Type Classes
  */
object ChapterOne {

  /*
    * Ex 1.3
    */
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


}
