package tests.feature

import scala.meta.internal.metals.{BuildInfo => V}
import tests.BaseDapSuite

class CrossDebugSuite extends BaseDapSuite("cross-debug") {

  override def scalaVersion: String = V.scala3

  assertBreakpoints(
    "outer",
    main = Some("a.helloWorld")
  )(
    source = """|/a/src/main/scala/a/Main.scala
                |package a
                |
                |@main 
                |def helloWorld(): Unit = {
                |>>println("Hello world")
                |  System.exit(0)
                |}
                |
                |""".stripMargin
  )

  assertBreakpoints(
    "outer-object",
    main = Some("a.helloWorld")
  )(
    source = """|/a/src/main/scala/a/Main.scala
                |package a
                |
                |@main 
                |def helloWorld(): Unit = {
                |  object Hello{
                |    def run() = {
                |>>    println("Hello world")
                |    }
                |  }
                |  Hello.run()
                |  System.exit(0)
                |}
                |
                |
                |""".stripMargin
  )

  // TODO
  // tests.feature.CrossDebugSuite.lambda
  // tests.feature.CrossDebugSuite.object-unapply
  // tests.feature.CrossDebugSuite.object-apply

}
