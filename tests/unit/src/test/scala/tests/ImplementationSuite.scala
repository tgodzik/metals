package tests
import scala.concurrent.Future

object ImplementationSuite extends BaseSlowSuite("implementation") {

  check(
    "basic",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait Livin@@gBeing
       |abstract class <<Animal>> extends LivingBeing
       |class <<Dog>> extends Animal
       |class <<Cat>> extends Animal
       |""".stripMargin
  )

  check(
    "advanced",
    """|/a/src/main/scala/a/LivingBeing.scala
       |package a
       |trait Livin@@gBeing
       |/a/src/main/scala/a/MadeOfAtoms.scala
       |package a
       |trait <<MadeOfAtoms>> extends LivingBeing
       |/a/src/main/scala/a/Animal.scala
       |package a
       |abstract class <<Animal>> extends LivingBeing
       |/a/src/main/scala/a/Dog.scala
       |package a
       |class <<Dog>> extends Animal with MadeOfAtoms
       |/a/src/main/scala/a/Cat.scala
       |package a
       |class <<Cat>> extends Animal
       |""".stripMargin
  )

  check(
    "nested",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait LivingBeing
       |abstract class Ani@@mal extends LivingBeing
       |object outer{
       |  object inner{  
       |    class <<Dog>> extends Animal
       |    class <<Cat>> extends Animal
       |    class Unrelated extends LivingBeing
       |  }
       |}
       |""".stripMargin
  )

  check(
    "basic-value",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait Math{
       |  def ze@@ro: Double
       |}
       |object WeirdMath extends Math{
       |  val <<zero>> = -1.0
       |}
       |""".stripMargin
  )

  check(
    "basic-var",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait Math{
       |  def ze@@ro: Double
       |}
       |object WeirdMath extends Math{
       |  var <<zero>> = -1.0
       |}
       |""".stripMargin
  )

  check(
    "nested",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait Math{
       |  def ze@@ro: Double
       |}
       |class Universe{
       |  object WeirdMath extends Math{
       |    def <<zero>> = {
       |      val a = 1.1
       |      val b = 3.2
       |      a + b
       |    }
       |  }
       |}
       |""".stripMargin
  )

  check(
    "basic-method",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait LivingBeing{
       |  def s@@ound: String
       |}
       |abstract class Animal extends LivingBeing{}
       |class Dog extends Animal{
       |  def <<sound>> = "woof"
       |  def other = 123
       |}
       |class Cat extends Animal{
       |  override def <<sound>> = "woof"
       |  def another(str : Long) = 123
       |}
       |""".stripMargin
  )

  check(
    "basic-method-params",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait LivingBeing{
       |  def sound: Int
       |  def s@@ound(times : Int): Int = 1
       |  def sound(start : Long): Int =  1
       |}
       |abstract class Animal extends LivingBeing{}
       |class Dog extends Animal{
       |  def sound = 1
       |  def sound(times : Long) = 1
       |  def <<sound>>(times : Int) = 1
       |}
       |class Cat extends Animal{
       |  override def <<sound>>(times : Int) = 1
       |  override def sound = 1
       |}
       |""".stripMargin
  )

  check(
    "long-method-params",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait A{
       |  def z@@ero(a : Int, b : Option[String])(c : Long, d: Double): Double = 0.0
       |  def zero(a : Long, b : Option[String])(c : Long, d: Double): Double = 0.0
       |}
       |class B extends A{
       |  def zero(a : Long, b : Option[String])(c : Long, d: Double): Double = 0.6
       |  def <<zero>>(a : Int, b : Option[String])(c : Long, d: Double): Double = 0.5
       |}
       |""".stripMargin
  )

  check(
    "generic-method",
    """|/a/src/main/scala/a/Main.scala
       |trait LivingObject {
       |  def so@@und[T](t: T): T
       |}
       |abstract class Animal extends LivingObject
       |object Cat extends Animal {
       |  override def <<sound>>[O](t: O): O = t
       |}
       |""".stripMargin
  )

  check(
    "generic-impl",
    """|/a/src/main/scala/a/Main.scala
       |trait Math[T] {
       |  def zer@@o(t: T): T
       |}
       |object IntegerMath extends Math[Int] {
       |  override def <<zero>>(t: Int): Int = 0
       |}
       |""".stripMargin
  )

  check(
    "generic-advanced",
    """|/a/src/main/scala/a/A.scala
       |trait A[S, R, T] {
       |  def meth@@od(s: S, r: R, t: T): T
       |}
       |/a/src/main/scala/a/B.scala
       |trait B[O] extends A[Int, O, Double]{
       |  def <<method>>(s: Int, r: O, t: Double): Double = ??? 
       |}
       |/a/src/main/scala/a/C.scala
       |class C extends B[Long] {
       |  override def <<method>>(s: Int, r: Long, t: Double): Double = ???
       |}
       |""".stripMargin
  )

  // TODO check type parameters and return

  // TODO needs scalameta update
  // check(
  //   "anon",
  //   """|/a/src/main/scala/a/Main.scala
  //      |package a
  //      |trait A@@nimal
  //      |object Main{
  //      |  val animal = new <<Animal>>{ def field(d : String) : Int = 123 }
  //      |}
  //      |""".stripMargin
  // )

  check(
    "anon-method",
    """|/a/src/main/scala/a/Main.scala
       |package a
       |trait Animal{ def soun@@d : String}
       |object Main{
       |  val animal = new Animal{ def <<sound>> = "|unknown|" }
       |}
       |""".stripMargin
  )

  def check(name: String, input: String): Unit = {
    val files = FileLayout.mapFromString(input)
    val (filename, edit) = files
      .find(_._2.contains("@@"))
      .map {
        case (fileName, code) =>
          (fileName, code.replaceAll("(<<|>>)", ""))
      }
      .getOrElse {
        throw new IllegalArgumentException(
          "No `@@` was defined that specifies cursor position"
        )
      }
    val expected = files.map {
      case (fileName, code) =>
        fileName -> code.replaceAll("@@", "")
    }
    val base = files.map {
      case (fileName, code) =>
        fileName -> code.replaceAll("(<<|>>|@@)", "")
    }

    testAsync(name) {
      cleanWorkspace()
      for {
        _ <- server.initialize(
          s"""/metals.json
             |{"a":{}}
             |${input
               .replaceAll("(<<|>>|@@)", "")}""".stripMargin
        )
        _ <- Future.sequence(
          files.map(file => server.didOpen(s"${file._1}"))
        )
        _ <- server.assertImplementation(
          filename,
          edit,
          expected.toMap,
          base.toMap
        )
      } yield ()
    }
  }
}
