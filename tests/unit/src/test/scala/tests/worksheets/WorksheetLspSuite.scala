package tests.worksheets

import scala.meta.internal.metals.{BuildInfo => V}

import munit.TestOptions

class WorksheetLspSuite extends tests.BaseWorksheetLspSuite(V.scala3) {

  override def versionSpecificCodeToValidate: String =
    """given str: String = """""

  override def versionSpecificScalacOptionsToValidate: List[String] = List(
    "-Ycheck-reentrant"
  )
  checkWorksheetDeps(
    "imports-inside",
    "a/src/main/scala/foo/Main.worksheet.sc",
  )

  checkWorksheetDeps("imports-outside", "Main.worksheet.sc")

  def checkWorksheetDeps(opts: TestOptions, path: String): Unit = {
    test(opts) {
      cleanWorkspace()
      for {
        _ <- initialize(
          s"""
             |/metals.json
             |{
             |  "a": {}
             |}
             |/$path
             |import $$dep.`com.lihaoyi::scalatags:0.12.0`
             |import scalatags.Text.all._
             |val htmlFile = html(
             |  body(
             |    p("This is a big paragraph of text")
             |  )
             |)
             |htmlFile.render
             |""".stripMargin
        )
        _ <- server.didOpen(path)
        _ <- server.didSave(path)(identity)
        identity <- server.completion(
          path,
          "htmlFile.render@@",
        )
        _ = assertNoDiff(
          server.workspaceDefinitions,
          s"""|/$path
              |import $$dep/*<no symbol>*/.`com.lihaoyi::scalatags:0.12.0`/*<no symbol>*/
              |import scalatags.Text/*Text.scala*/.all/*Text.scala*/._
              |val htmlFile/*L2*/ = html/*Tags.scala*/(
              |  body/*Tags.scala*/(
              |    p/*Tags.scala*/("This is a big paragraph of text")
              |  )
              |)
              |htmlFile/*L2*/.render/*Text.scala*/
              |""".stripMargin,
        )
        _ <- server.didOpen("scalatags/Text.scala")
        _ = assertNoDiff(identity, "render: String")
        _ = assertNoDiagnostics()
        _ = assertNoDiff(
          client.workspaceDecorations,
          """|import $dep.`com.lihaoyi::scalatags:0.12.0`
             |import scalatags.Text.all._
             |val htmlFile = html(
             |  body(
             |    p("This is a big paragraph of text")
             |  )
             |) // : TypedTag[String] = <html><body><p>This is a big paragraph of text</p></body></html>
             |htmlFile.render // : String = <html><body><p>This is a big paragraph of text</p></body></html>
             |""".stripMargin,
        )
      } yield ()
    }
  }

  test("bad-dep") {
    cleanWorkspace()
    val path = "hi.worksheet.sc"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {}
           |}
           |/${path}
           |import $$dep.`com.lihaoyi::scalatags:0.999.0`
           |""".stripMargin
      )
      _ <- server.didOpen(path)
      _ = assertNoDiff(
        client.workspaceErrorShowMessages,
        "Error downloading com.lihaoyi:scalatags_3:0.999.0",
      )
    } yield ()
  }
  // Ensure that on Java +9 that all modules are correctly loaded with the Mdoc
  // classloader including things like the java.sql module.
  // https://github.com/scalameta/metals/issues/2187
  test("classloader") {
    cleanWorkspace()
    val path = "hi.worksheet.sc"
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {}
           |}
           |/${path}
           |new java.sql.Date(100L)
           |""".stripMargin
      )
      _ <- server.didOpen(path)
      _ = assertNoDiff(
        client.workspaceDecorations,
        "new java.sql.Date(100L) // : Date = 1970-01-01",
      )
    } yield ()
  }

  test("literals") {
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "${V.scala213}"}}
           |/a/src/main/scala/foo/Main.worksheet.sc
           |val literal: 42 = 42
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")(identity)
      _ = assertNoDiagnostics()
    } yield ()
  }
}
