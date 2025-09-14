package tests.inlayHints

import scala.meta.internal.metals.{BuildInfo => V}

import tests.BaseInlayHintsLspSuite

class CommentEvalLspSuite
    extends BaseInlayHintsLspSuite("comment-eval", V.scala213) {

  check(
    "basic-eval",
    """|object Main {
       |  val x: Int = 42
       |  // >>> 42 + 1/* // : Int = 43*/
       |}
       |""".stripMargin,
  )

  check(
    "consecutive-statements",
    """|object Main {
       |  // >>> val a = 10/* // : Int = 10*/
       |  // >>> val b = 20/* // : Int = 20*/
       |  // >>> a + b/* // : Int = 30*/
       |  
       |  val separator/*: String<<java/lang/String#>>*/ = "---"
       |  
       |  // >>> val c = 5/* // : Int = 5*/
       |  // >>> c * 3/* // : Int = 15*/
       |}
       |""".stripMargin,
  )

  check(
    "math-operations",
    """|object Main {
       |  // >>> 2 + 2/* // : Int = 4*/
       |  // >>> Math.PI * 2/* // : Double = 6.283185307179586*/
       |  // >>> List(1, 2, 3, 4).filter(_ % 2 == 0)/* // : List[Int] = List(2, 4)*/
       |  // >>> "abc".reverse/* // : String = "cba"*/
       |}
       |""".stripMargin,
  )

  check(
    "mixed-with-regular-comments",
    """|object Main {
       |  // This is a regular comment
       |  val x/*: Int<<scala/Int#>>*/ = 42
       |  // >>> x * 2/* // : Int = 84*/
       |  
       |  // >>> x + 10/* // : Int = 52*/
       |  // Regular comment without >>>
       |}
       |""".stripMargin,
  )
  check(
    "multiple-objects",
    """|object Main {
       |  // This is a regular comment
       |  val x/*: Int<<scala/Int#>>*/ = 42
       |}
       |
       |object Main2 {
       |  // This is a regular comment
       |  val x/*: Int<<scala/Int#>>*/ = 42
       |  // >>> x * 2/* // : Int = 84*/
       |  
       |  // >>> x + 10/* // : Int = 52*/
       |  // Regular comment without >>>
       |}
       |""".stripMargin,
  )
  check(
    "outside-val",
    """|object Main {
       |  // This is a regular comment
       |  val x/*: Int<<scala/Int#>>*/ = 42
       |  // >>> x * 2/* // : Int = 84*/
       |}
       |
       |val y/*: Int<<scala/Int#>>*/ = 123
       |
       |// >>> y + 10/* // : Int = 133*/
       |""".stripMargin,
    scalaVersion = V.scala3,
  )
}
