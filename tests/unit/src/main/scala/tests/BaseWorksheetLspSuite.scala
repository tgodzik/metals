package tests

import scala.concurrent.Promise
import scala.concurrent.duration._

import scala.meta.internal.metals.InitializationOptions
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.codeactions.CreateNewSymbol
import scala.meta.internal.metals.codeactions.ImportMissingSymbol
import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.internal.semver.SemVer

abstract class BaseWorksheetLspSuite(
    scalaVersion: String
) extends BaseLspSuite(s"worksheet") {

  override protected def initializationOptions: Option[InitializationOptions] =
    Some(
      InitializationOptions.Default.copy(
        decorationProvider = Some(true)
      )
    )

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      worksheetScreenWidth = 40,
      worksheetCancelTimeout = 1,
      fallbackScalaVersion = Some(scalaVersion),
    )

  override def munitIgnore: Boolean = !isValidScalaVersionForEnv(scalaVersion)

  def versionSpecificCodeToValidate: String = ""

  /**
   * These options when provided should not break worksheets.
   */
  def versionSpecificScalacOptionsToValidate: List[String] = Nil

  // sourcecode is not yet published for Scala 3
  if (!ScalaVersions.isScala3Version(scalaVersion))
    test("completion") {
      assume(!isWindows, "This test is flaky on Windows")
      cleanWorkspace()
      for {
        _ <- initialize(
          s"""
             |/metals.json
             |{
             |  "a": {
             |    "scalaVersion": "$scalaVersion",
             |    "libraryDependencies": ["com.lihaoyi::sourcecode:0.2.1"]
             |  }
             |}
             |/a/src/main/scala/foo/Main.worksheet.sc
             |identity(42)
             |val name = sourcecode.Name.generate.value
             |""".stripMargin
        )
        _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
        _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
        identity <- server.completion(
          "a/src/main/scala/foo/Main.worksheet.sc",
          "identity@@",
        )
        _ = assertNoDiff(identity, "identity[A](x: A): A")
        generate <- server.completion(
          "a/src/main/scala/foo/Main.worksheet.sc",
          "generate@@",
        )
        _ = assertNoDiff(
          generate,
          getExpected(
            "generate: Name",
            Map("3" -> "generate=> sourcecode.Name"),
            scalaVersion,
          ),
        )
        _ = assertNoDiagnostics()
        _ <- server.assertInlayHints(
          "a/src/main/scala/foo/Main.worksheet.sc",
          getExpected(
            """|identity(42)/* // : Int = 42*/
               |val name = sourcecode.Name.generate.value/* // : String = "name"*/
               |""".stripMargin,
            Map(
              "3" ->
                """|identity(42)/* // : Int = 42*/
                   |val name = sourcecode.Name.generate.value/* // : String = name*/
                   |""".stripMargin
            ),
            scalaVersion,
          ),
        )
      } yield ()
    }

  test("ANSI") {
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/Main.worksheet.sc
           |pprint.pprintln("Hello, world!")
           |""".stripMargin
      )
      _ <- server.didOpen("a/Main.worksheet.sc")
      _ <- server.assertInlayHints(
        "a/Main.worksheet.sc",
        """|pprint.pprintln("Hello, world!")/* // "Hello, world!"*/
           |""".stripMargin,
      )
    } yield ()
  }

  test("outside-target") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/Main.worksheet.sc
           |import java.nio.file.Files
           |val name = "Susan"
           |val greeting = s"Hello $$name"
           |println(greeting + "\\nHow are you?")
           |1.to(10).toVector
           |val List(a, b) = List(42, 10)
           |""".stripMargin + versionSpecificCodeToValidate
      )
      _ <- server.didOpen("a/Main.worksheet.sc")
      _ = assertNoDiagnostics()
      _ <- server.assertInlayHints(
        "a/Main.worksheet.sc",
        getExpected(
          """|import java.nio.file.Files
             |val name = "Susan"/* // : String = "Susan"*/
             |val greeting = s"Hello $name"/* // : String = "Hello Susan"*/
             |println(greeting + "\nHow are you?")/* // Hello Susan…*/
             |1.to(10).toVector/* // : Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)*/
             |val List(a, b) = List(42, 10)/* // a: Int = 42, b: Int = 10*/
             |""".stripMargin,
          Map(
            "3" ->
              """|import java.nio.file.Files
                 |val name = "Susan"/* // : String = Susan*/
                 |val greeting = s"Hello $name"/* // : String = Hello Susan*/
                 |println(greeting + "\nHow are you?")/* // Hello Susan…*/
                 |1.to(10).toVector/* // : Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)*/
                 |val List(a, b) = List(42, 10)/* // a: Int = 42, b: Int = 10*/
                 |""".stripMargin
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("render") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/Main.worksheet.sc
           |import java.nio.file.Files
           |val name = "Susan"
           |val greeting = s"Hello $$name"
           |println(greeting + "\\nHow are you?")
           |1.to(10).toVector
           |val List(a, b) = List(42, 10)
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ = assertNoDiagnostics()
      _ <- server.assertInlayHints(
        "a/src/main/scala/Main.worksheet.sc",
        getExpected(
          """|import java.nio.file.Files
             |val name = "Susan"/* // : String = "Susan"| name: String = "Susan" |*/
             |val greeting = s"Hello $name"/* // : String = "Hello Susan"| greeting: String = "Hello Susan" |*/
             |println(greeting + "\nHow are you?")/* // Hello Susan…| // Hello Susan\n// How are you? |*/
             |1.to(10).toVector/* // : Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)| res1: Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) |*/
             |val List(a, b) = List(42, 10)/* // a: Int = 42, b: Int = 10| a: Int = 42\nb: Int = 10 |*/
             |""".stripMargin,
          Map(
            "3" ->
              """|import java.nio.file.Files
                 |val name = "Susan"/* // : String = Susan| name: String = Susan |*/
                 |val greeting = s"Hello $name"/* // : String = Hello Susan| greeting: String = Hello Susan |*/
                 |println(greeting + "\nHow are you?")/* // Hello Susan…| // Hello Susan\n// How are you? |*/
                 |1.to(10).toVector/* // : Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)| res1: Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) |*/
                 |val List(a, b) = List(42, 10)/* // a: Int = 42, b: Int = 10| a: Int = 42\nb: Int = 10 |*/
                 |""".stripMargin
          ),
          scalaVersion,
        ),
        withTooltip = true,
      )
    } yield ()
  }

  test("cancel") {
    cleanWorkspace()
    val cancelled = Promise[Unit]()
    client.onWorkDoneProgressStart = { (message, cancelParams) =>
      if (message.startsWith("Evaluating worksheet")) {
        cancelled.trySuccess(())
        server.fullServer.didCancelWorkDoneProgress(cancelParams)
      }
    }

    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/Main.worksheet.sc
           |println(42)
           |Stream.from(10).last
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ <- cancelled.future.withTimeout(10.seconds)
      _ = client.onWorkDoneProgressStart = (_, _) => {}
      _ <- server.didChange("a/src/main/scala/Main.worksheet.sc")(
        _.replace("Stream", "// Stream")
      )
      _ <- server.didSave("a/src/main/scala/Main.worksheet.sc")
      _ <- server.didChange("a/src/main/scala/Main.worksheet.sc")(
        _.replace("42", "43")
      )
      _ <- server.didSave("a/src/main/scala/Main.worksheet.sc")
      _ <- server.assertInlayHints(
        "a/src/main/scala/Main.worksheet.sc",
        """|
           |println(43)/* // 43*/
           |// Stream.from(10).last
           |""".stripMargin,
      )
    } yield ()
  }

  test("crash") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/Main.worksheet.sc
           |val x = 42
           |throw new RuntimeException("boom")
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ <- server.assertInlayHints(
        "a/src/main/scala/Main.worksheet.sc",
        """|val x = 42/* // : Int = 42*/
           |throw new RuntimeException("boom")
           |""".stripMargin,
      )
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        getExpected(
          """|a/src/main/scala/Main.worksheet.sc:2:1: error: java.lang.RuntimeException: boom
             |	at repl.MdocSession$MdocApp.<init>(Main.worksheet.sc:11)
             |	at repl.MdocSession$.app(Main.worksheet.sc:3)
             |
             |throw new RuntimeException("boom")
             |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             |""".stripMargin,
          Map(
            "3" ->
              """|a/src/main/scala/Main.worksheet.sc:2:1: error: java.lang.RuntimeException: boom
                 |	at repl.MdocSession$MdocApp.<init>(Main.worksheet.sc:12)
                 |	at repl.MdocSession$.app(Main.worksheet.sc:3)
                 |
                 |throw new RuntimeException("boom")
                 |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 |""".stripMargin
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("dependsOn") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {"scalaVersion": "$scalaVersion"},
           |  "b": {"dependsOn": ["a"], "scalaVersion": "$scalaVersion"}
           |}
           |/a/src/main/scala/core/Lib.scala
           |package core
           |case object Lib
           |/b/src/main/scala/core/Lib2.scala
           |package core
           |case object Lib2
           |/b/src/main/scala/foo/Main.worksheet.sc
           |println(core.Lib)
           |println(core.Lib2)
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/core/Lib.scala")
      _ <- server.didOpen("b/src/main/scala/core/Lib2.scala")
      _ <- server.didOpen("b/src/main/scala/foo/Main.worksheet.sc")
      _ = assertNoDiagnostics()
      _ <- server.assertInlayHints(
        "b/src/main/scala/foo/Main.worksheet.sc",
        """|println(core.Lib)/* // Lib*/
           |println(core.Lib2)/* // Lib2*/
           |""".stripMargin,
      )
    } yield ()
  }

  test("no-worksheet".flaky) {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a": {"scalaVersion": "$scalaVersion"}}
            |/a/src/main/scala/Main.sc
            |identity(42)
            |val x: Int = ""
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.sc")
      _ = assertNoDiagnostics()
      identity <- server.completion(
        "a/src/main/scala/Main.sc",
        "identity@@",
      )
      // completions work despite error
      _ = assertNoDiff(identity, "identity[A](x: A): A")
      // worksheet evaluations do not appear for non ".worksheet.sc" files.
      _ <- server.assertInlayHints(
        "b/src/main/scala/foo/Main.worksheet.sc",
        """|identity(42)
           |val x: Int = ""
           |""".stripMargin,
      )
    } yield ()
  }

  test("multi-line-string") {
    cleanWorkspace()
    val tripleQuotes = (1 to 3).map(_ => '"').mkString
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a": {"scalaVersion": "$scalaVersion"}}
            |/a/src/main/scala/Main.worksheet.sc
            |s${tripleQuotes}|# Spear $${2.max(1)}
            ||smth: $${9.max(8)}
            ||${tripleQuotes}.stripMargin
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ = assertNoDiagnostics()
      max <- server.completion(
        "a/src/main/scala/Main.worksheet.sc",
        s"|smth: $${9.max@@",
      )
      // completions work despite error
      _ = assertNoDiff(
        max,
        getExpected(
          s"""|max(that: Double): Double
              |max(that: Float): Float
              |max(that: Int): Int
              |max(that: Long): Long
              |""".stripMargin,
          Map(
            "3" ->
              s"""|max(that: Int): Int
                  |""".stripMargin
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("update-classpath") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/a/Util.scala
           |package a
           |object Util {
           |  def increase(n: Int): Int = n + 1
           |}
           |/a/src/main/scala/a/Main.worksheet.sc
           |a.Util.increase(1)
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/a/Util.scala")
      _ <- server.didOpen("a/src/main/scala/a/Main.worksheet.sc")
      _ = assertNoDiagnostics()
      _ <- server.assertInlayHints(
        "a/src/main/scala/a/Main.worksheet.sc",
        """
          |a.Util.increase(1)/* // : Int = 2*/
          |""".stripMargin,
      )
      _ <- server.didChange("a/src/main/scala/a/Util.scala")(
        _.replace("n + 1", "n + 2")
      )
      _ <- server.didSave("a/src/main/scala/a/Util.scala")
      _ <- server.didFocus("a/src/main/scala/a/Main.worksheet.sc")
      _ <- server.assertInlayHints(
        "a/src/main/scala/a/Main.worksheet.sc",
        """
          |a.Util.increase(1)/* // : Int = 3*/
          |""".stripMargin,
      )
    } yield ()
  }

  test("syntax-error") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{"a": {"scalaVersion": "$scalaVersion"}}
            |/a/src/main/scala/a/Main.worksheet.sc
            |val x: Int = ""
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/a/Main.worksheet.sc")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        getExpected(
          """|a/src/main/scala/a/Main.worksheet.sc:1:14: error: type mismatch;
             | found   : String("")
             | required: Int
             |val x: Int = ""
             |             ^^
             |""".stripMargin,
          Map(
            "3" ->
              """|a/src/main/scala/a/Main.worksheet.sc:1:14: error:
                 |Found:    ("" : String)
                 |Required: Int
                 |val x: Int = ""
                 |             ^^
                 |""".stripMargin
          ),
          scalaVersion,
        ),
      )
      _ <- server.didClose("a/src/main/scala/a/Main.worksheet.sc")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        "",
      )
      _ <- server.didOpen("a/src/main/scala/a/Main.worksheet.sc")
      _ <- server.didChange("a/src/main/scala/a/Main.worksheet.sc")(
        _.replace("val x", "def y = \nval x")
      )
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        getExpected(
          """|a/src/main/scala/a/Main.worksheet.sc:2:1: error: illegal start of simple expression
             |val x: Int = ""
             |^^^
             |a/src/main/scala/a/Main.worksheet.sc:2:14: error: type mismatch;
             | found   : String("")
             | required: Int
             |val x: Int = ""
             |             ^^
             |""".stripMargin,
          Map(
            "3" ->
              """|a/src/main/scala/a/Main.worksheet.sc:2:1: error: illegal start of simple expression
                 |val x: Int = ""
                 |^^^
                 |a/src/main/scala/a/Main.worksheet.sc:2:14: error:
                 |Found:    ("" : String)
                 |Required: Int
                 |val x: Int = ""
                 |             ^^
                 |""".stripMargin
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("definition", withoutVirtualDocs = false) {
    // NOTE(olafur) this test fails unpredicatly on Windows with
    //      """|/a/src/main/scala/Main.worksheet.sc
    //         |val message/*<no symbol>*/ = "Hello World!"
    //         |println/*<no symbol>*/(message/*<no symbol>*/)
    assume(!isWindows, "This test fails unpredictably on Window")
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/Main.worksheet.sc
           |val message = "Hello World!"
           |println(message)
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ = assertNoDiff(
        server.workspaceDefinitions,
        """|/a/src/main/scala/Main.worksheet.sc
           |val message/*L0*/ = "Hello World!"
           |println/*Predef.scala*/(message/*L0*/)
           |""".stripMargin,
      )
    } yield ()
  }

  test("root-outside-definition") {
    assume(!isWindows, "This test fails unpredictably on Window")
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/Main.worksheet.sc
           |import java.time.Instant
           |
           |val x = Instant.now()
           |val y = List.fill(2)(2)
           |""".stripMargin
      )
      _ <- server.didOpen("Main.worksheet.sc")
      _ = assertNoDiff(
        server.workspaceDefinitions,
        getExpected(
          s"""|/Main.worksheet.sc
              |import java.time.Instant/*Instant.java*/
              |
              |val x/*L2*/ = Instant/*Instant.java*/.now/*Instant.java*/()
              |val y/*L3*/ = List/*List.scala*/.fill/*GenTraversableFactory.scala*/(2)(2)
              |""".stripMargin,
          Map(
            V.scala213 ->
              s"""|/Main.worksheet.sc
                  |import java.time.Instant/*Instant.java*/
                  |
                  |val x/*L2*/ = Instant/*Instant.java*/.now/*Instant.java*/()
                  |val y/*L3*/ = List/*package.scala*/.fill/*Factory.scala*/(2)(2)
                  |""".stripMargin,
            "3" ->
              """|/Main.worksheet.sc
                 |import java.time.Instant/*Instant.java*/
                 |
                 |val x/*L2*/ = Instant/*Instant.java*/.now/*Instant.java*/()
                 |val y/*L3*/ = List/*package.scala*/.fill/*Factory.scala*/(2)(2)
                 |""".stripMargin,
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("no-position") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/Main.worksheet.sc
           |type Structural = {
           |  def foo(): Int
           |}
           |class Foo { def foo(): Int = 42 }
           |new Foo().asInstanceOf[Structural].foo()
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/Main.worksheet.sc")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        getExpected(
          """|a/src/main/scala/Main.worksheet.sc:1:1: warning: one feature warning; re-run with -feature for details
             |type Structural = {
             |^
             |""".stripMargin,
          compat = Map(
            V.scala213 ->
              """|a/src/main/scala/Main.worksheet.sc:1:1: warning: 1 feature warning; re-run with -feature for details
                 |type Structural = {
                 |^
                 |""".stripMargin,
            "3" ->
              """|a/src/main/scala/Main.worksheet.sc:5:1: error:
                 |Found:    MdocApp.this.Structural
                 |Required: Selectable | Dynamic
                 |new Foo().asInstanceOf[Structural].foo()
                 |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 |""".stripMargin,
          ),
          scalaVersion,
        ),
      )
    } yield ()
  }

  test("fatal-exception") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "$scalaVersion"}}
           |/a/src/main/scala/StackOverflowError.worksheet.sc
           |throw new StackOverflowError()
           |/a/src/main/scala/NoSuchMethodError.worksheet.sc
           |throw new NoSuchMethodError()
           |/a/src/main/scala/IncompatibleClassChangeError.worksheet.sc
           |throw new IncompatibleClassChangeError()
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/StackOverflowError.worksheet.sc")
      _ <- server.didOpen(
        "a/src/main/scala/IncompatibleClassChangeError.worksheet.sc"
      )
      _ <- server.didOpen("a/src/main/scala/NoSuchMethodError.worksheet.sc")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """|a/src/main/scala/IncompatibleClassChangeError.worksheet.sc:1:1: error: java.lang.IncompatibleClassChangeError
           |	at repl.MdocSession$MdocApp.<init>(IncompatibleClassChangeError.worksheet.sc:8)
           |	at repl.MdocSession$.app(IncompatibleClassChangeError.worksheet.sc:3)
           |
           |throw new IncompatibleClassChangeError()
           |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           |a/src/main/scala/NoSuchMethodError.worksheet.sc:1:1: error: java.lang.NoSuchMethodError
           |	at repl.MdocSession$MdocApp.<init>(NoSuchMethodError.worksheet.sc:8)
           |	at repl.MdocSession$.app(NoSuchMethodError.worksheet.sc:3)
           |
           |throw new NoSuchMethodError()
           |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           |a/src/main/scala/StackOverflowError.worksheet.sc:1:1: error: java.lang.StackOverflowError
           |	at repl.MdocSession$MdocApp.<init>(StackOverflowError.worksheet.sc:8)
           |	at repl.MdocSession$.app(StackOverflowError.worksheet.sc:3)
           |
           |throw new StackOverflowError()
           |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           |""".stripMargin,
      )
    } yield ()
  }

  test("export") {
    assume(!isWindows, "This test is flaky on Windows")
    cleanWorkspace()

    val opts = versionSpecificScalacOptionsToValidate
      .map(opt => s"\"$opt\"")
      .mkString(",")
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{"a": {"scalaVersion": "${scalaVersion}", "scalacOptions": [$opts]}}
           |/a/src/main/scala/foo/Main.worksheet.sc
           |case class Hi(a: Int, b: Int, c: Int)
           |val hi1 =
           |  Hi(1, 2, 3)
           |val hi2 = Hi(4, 5, 6)
           |
           |val hellos = List(hi1, hi2)
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
      export = server.exportEvaluation(
        "a/src/main/scala/foo/Main.worksheet.sc"
      )
      _ = assertEquals(
        export,
        Some(
          getExpected(
            """|
               |case class Hi(a: Int, b: Int, c: Int)
               |val hi1 =
               |  Hi(1, 2, 3)
               |// hi1: Hi = Hi(1, 2, 3)
               |val hi2 = Hi(4, 5, 6)
               |// hi2: Hi = Hi(4, 5, 6)
               |
               |val hellos = List(hi1, hi2)
               |// hellos: List[Hi] = List(Hi(1, 2, 3), Hi(4, 5, 6))""".stripMargin,
            Map(
              V.scala213 -> """|
                               |case class Hi(a: Int, b: Int, c: Int)
                               |val hi1 =
                               |  Hi(1, 2, 3)
                               |// hi1: Hi = Hi(a = 1, b = 2, c = 3)
                               |val hi2 = Hi(4, 5, 6)
                               |// hi2: Hi = Hi(a = 4, b = 5, c = 6)
                               |
                               |val hellos = List(hi1, hi2)
                               |// hellos: List[Hi] = List(Hi(a = 1, b = 2, c = 3), Hi(a = 4, b = 5, c = 6))""".stripMargin,
              "3" -> """|
                        |case class Hi(a: Int, b: Int, c: Int)
                        |val hi1 =
                        |  Hi(1, 2, 3)
                        |// hi1: Hi = Hi(1,2,3)
                        |val hi2 = Hi(4, 5, 6)
                        |// hi2: Hi = Hi(4,5,6)
                        |
                        |val hellos = List(hi1, hi2)
                        |// hellos: List[Hi] = List(Hi(1,2,3), Hi(4,5,6))""".stripMargin,
            ),
            scalaVersion,
          )
        ),
      )
      _ <- server.didChange("a/src/main/scala/foo/Main.worksheet.sc")(
        _.replace(
          "Hi(1, 2, 3)",
          "Hi(7, 8, 9)",
        )
      )
      _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
      export = server.exportEvaluation(
        "a/src/main/scala/foo/Main.worksheet.sc"
      )
      _ = assertEquals(
        export,
        Some(
          getExpected(
            """|
               |case class Hi(a: Int, b: Int, c: Int)
               |val hi1 =
               |  Hi(7, 8, 9)
               |// hi1: Hi = Hi(7, 8, 9)
               |val hi2 = Hi(4, 5, 6)
               |// hi2: Hi = Hi(4, 5, 6)
               |
               |val hellos = List(hi1, hi2)
               |// hellos: List[Hi] = List(Hi(7, 8, 9), Hi(4, 5, 6))""".stripMargin,
            Map(
              V.scala213 -> """|
                               |case class Hi(a: Int, b: Int, c: Int)
                               |val hi1 =
                               |  Hi(7, 8, 9)
                               |// hi1: Hi = Hi(a = 7, b = 8, c = 9)
                               |val hi2 = Hi(4, 5, 6)
                               |// hi2: Hi = Hi(a = 4, b = 5, c = 6)
                               |
                               |val hellos = List(hi1, hi2)
                               |// hellos: List[Hi] = List(Hi(a = 7, b = 8, c = 9), Hi(a = 4, b = 5, c = 6))""".stripMargin,
              "3" -> """|
                        |case class Hi(a: Int, b: Int, c: Int)
                        |val hi1 =
                        |  Hi(7, 8, 9)
                        |// hi1: Hi = Hi(7,8,9)
                        |val hi2 = Hi(4, 5, 6)
                        |// hi2: Hi = Hi(4,5,6)
                        |
                        |val hellos = List(hi1, hi2)
                        |// hellos: List[Hi] = List(Hi(7,8,9), Hi(4,5,6))""".stripMargin,
            ),
            scalaVersion,
          )
        ),
      )
    } yield ()

  }

  test("ivy-completion") {
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {
           |    "scalaVersion": "${scalaVersion}"
           |  }
           |}
           |/a/src/main/scala/foo/Main.worksheet.sc
           |import $$ivy.`io.cir`
           |import $$dep.`io.circe::circe-ref`
           |import $$dep.`com.lihaoyi::upickle:1.4`
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
      groupExpectedCompletionList =
        """|io.circe
           |io.circul""".stripMargin
      groupCompletionList <- server.completion(
        "a/src/main/scala/foo/Main.worksheet.sc",
        "import $ivy.`io.cir@@`",
      )
      _ = assertNoDiff(groupCompletionList, groupExpectedCompletionList)

      artefactExpectedCompletionList = getExpected(
        """|circe-refined
           |circe-refined_native0.4
           |circe-refined_native0.5
           |circe-refined_sjs0.6
           |circe-refined_sjs1
           |""".stripMargin,
        Map(
          "3" -> """|circe-refined
                    |circe-refined_native0.4
                    |circe-refined_native0.5
                    |circe-refined_sjs1
                    |""".stripMargin
        ),
        scalaVersion,
      )
      artefactCompletionList <- server.completion(
        "a/src/main/scala/foo/Main.worksheet.sc",
        "import $dep.`io.circe::circe-ref@@`",
      )
      _ = assertNoDiff(artefactCompletionList, artefactExpectedCompletionList)

      versionExpectedCompletionList =
        List("1.4.4", "1.4.3", "1.4.2", "1.4.1", "1.4.0")
      response <- server.completionList(
        "a/src/main/scala/foo/Main.worksheet.sc",
        "import $dep.`com.lihaoyi::upickle:1.4@@`",
      )
      versionCompletionList = response
        .getItems()
        .asScala
        .map(_.getLabel())
        .toList
      _ = assertEquals(versionCompletionList, versionExpectedCompletionList)
      noCompletions <- server.completion(
        "a/src/main/scala/foo/Main.worksheet.sc",
        "import $dep.`com.lihaoyi::upickle:1.4`@@",
      )
      _ = assertNoDiff(noCompletions, "")
    } yield ()
  }

  if (
    SemVer.isLaterVersion("3.7.0", scalaVersion) || !ScalaVersions
      .isScala3Version(scalaVersion)
  )
    test("using-completion") {
      for {
        _ <- initialize(
          s"""
             |/metals.json
             |{
             |  "a": {
             |    "scalaVersion": "${scalaVersion}"
             |  }
             |}
             |/a/src/main/scala/foo/Main.worksheet.sc
             |//> using dep io.cir
             |//> using dep io.circe::circe-ref
             |//> using dep com.lihaoyi::upickle:1.4
             |""".stripMargin
        )
        _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
        groupCompletionList <- server.completion(
          "a/src/main/scala/foo/Main.worksheet.sc",
          "//> using dep io.cir@@",
        )
        _ = assertNoDiff(
          groupCompletionList,
          """|io.circe
             |io.circul""".stripMargin,
        )

        artefactExpectedCompletionList = getExpected(
          """|circe-refined
             |circe-refined_native0.4
             |circe-refined_native0.5
             |circe-refined_sjs0.6
             |circe-refined_sjs1
             |""".stripMargin,
          Map(
            "3" -> """|circe-refined
                      |circe-refined_native0.4
                      |circe-refined_native0.5
                      |circe-refined_sjs1
                      |""".stripMargin
          ),
          scalaVersion,
        )
        artefactCompletionList <- server.completion(
          "a/src/main/scala/foo/Main.worksheet.sc",
          "//> using dep io.circe::circe-ref@@",
        )
        _ = assertNoDiff(artefactCompletionList, artefactExpectedCompletionList)

        versionExpectedCompletionList =
          List("1.4.4", "1.4.3", "1.4.2", "1.4.1", "1.4.0")
        response <- server.completionList(
          "a/src/main/scala/foo/Main.worksheet.sc",
          "//> using dep com.lihaoyi::upickle:1.4@@",
        )
        versionCompletionList = response
          .getItems()
          .asScala
          .map(_.getLabel())
          .toList
        _ = assertEquals(versionCompletionList, versionExpectedCompletionList)
      } yield ()
    }

  if (ScalaVersions.isScala3Version(scalaVersion))
    test("import-missing-symbol") {
      cleanWorkspace()
      val path = "a/src/main/scala/foo/Main.worksheet.sc"
      val expectedActions =
        if (SemVer.isLaterVersion("3.7.0", scalaVersion))
          s"""|${ImportMissingSymbol.title("Future", "scala.concurrent")}
              |${CreateNewSymbol.title("Future")}
              |""".stripMargin
        else
          s"""|${ImportMissingSymbol.title("Future", "scala.concurrent")}
              |${ImportMissingSymbol.title("Future", "java.util.concurrent")}
              |${CreateNewSymbol.title("Future")}
              |""".stripMargin
      for {
        _ <- initialize(
          s"""
             |/metals.json
             |{"a": {"scalaVersion": "${scalaVersion}"}}
             |/$path
             |//> using scala $scalaVersion
             |
             |// Some comment
             |
             |// Object comment
             |object A {
             |  val f = Future.successful(42)
             |}
             |""".stripMargin
        )
        _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
        _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
        codeActions <-
          server
            .assertCodeAction(
              path,
              s"""|//> using scala $scalaVersion
                  |
                  |// Some comment
                  |
                  |// Object comment
                  |object A {
                  |  val f = <<Future>>.successful(42)
                  |}
                  |""".stripMargin,
              expectedActions,
              Nil,
            )
        _ <- client.applyCodeAction(0, codeActions, server)
        _ <- server.didSave(path)
        // Assert if indentation is correct. See `AutoImports.renderImport`
        _ = assertNoDiff(
          server.bufferContents(path),
          s"""|//> using scala $scalaVersion
              |
              |// Some comment
              |import scala.concurrent.Future
              |
              |// Object comment
              |object A {
              |  val f = Future.successful(42)
              |}
              |""".stripMargin,
        )
      } yield ()
    }

  test("semantic-highlighting") {
    val tripleQ = "\"\"\""
    val expected =
      if (scalaVersion == V.scala212)
        s"""|<<case>>/*keyword*/ <<class>>/*keyword*/ <<Hi>>/*class*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<c>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
            |<<val>>/*keyword*/ <<hi1>>/*variable,definition,readonly*/ =
            |  <<Hi>>/*class*/(<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/)
            |<<val>>/*keyword*/ <<hi2>>/*variable,definition,readonly*/ = <<Hi>>/*class*/(<<4>>/*number*/, <<5>>/*number*/, <<6>>/*number*/)
            |
            |<<val>>/*keyword*/ <<hellos>>/*variable,definition,readonly*/ = <<List>>/*class*/(<<hi1>>/*variable,readonly*/, <<hi2>>/*variable,readonly*/)
            |<<val>>/*keyword*/ <<str>>/*variable,definition,readonly*/ = <<$tripleQ>>/*string*/
            |<<  hello>>/*string*/
            |<<  world$tripleQ>>/*string*/.<<stripMargin>>/*method*/
            |""".stripMargin
      else
        s"""|<<case>>/*keyword*/ <<class>>/*keyword*/ <<Hi>>/*class*/(<<a>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<b>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/, <<c>>/*variable,declaration,readonly*/: <<Int>>/*class,abstract*/)
            |<<val>>/*keyword*/ <<hi1>>/*variable,definition,readonly*/ =
            |  <<Hi>>/*class*/(<<1>>/*number*/, <<2>>/*number*/, <<3>>/*number*/)
            |<<val>>/*keyword*/ <<hi2>>/*variable,definition,readonly*/ = <<Hi>>/*class*/(<<4>>/*number*/, <<5>>/*number*/, <<6>>/*number*/)
            |
            |<<val>>/*keyword*/ <<hellos>>/*variable,definition,readonly*/ = <<List>>/*class*/(<<hi1>>/*variable,readonly*/, <<hi2>>/*variable,readonly*/)
            |<<val>>/*keyword*/ <<str>>/*variable,definition,readonly*/ = <<$tripleQ>>/*string*/
            |<<  hello>>/*string*/
            |<<  world$tripleQ>>/*string*/.<<stripMargin>>/*method*/
            |""".stripMargin

    val fileContent =
      TestSemanticTokens.removeSemanticHighlightDecorations(expected)
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {
           |    "scalaVersion": "$scalaVersion"
           |  }
           |}
           |/a/src/main/scala/foo/Main.worksheet.sc
           |$fileContent
           |""".stripMargin
      )
      _ <- server.didChangeConfiguration(
        """{
          |  "enable-semantic-highlighting": true
          |}
          |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.assertSemanticHighlight(
        "a/src/main/scala/foo/Main.worksheet.sc",
        expected,
        fileContent,
      )
    } yield ()
  }

  test("semantic-highlighting2") {
    val expected =
      s"""|
          |<<val>>/*keyword*/ <<hellos>>/*variable,definition,readonly*/ = <<List>>/*class*/(hi1, hi2)
          |""".stripMargin

    val fileContent =
      TestSemanticTokens.removeSemanticHighlightDecorations(expected)
    for {
      _ <- initialize(
        s"""
           |/metals.json
           |{
           |  "a": {
           |    "scalaVersion": "$scalaVersion"
           |  }
           |}
           |/a/src/main/scala/foo/Main.worksheet.sc
           |$fileContent
           |""".stripMargin
      )
      _ <- server.didChangeConfiguration(
        """{
          |  "enable-semantic-highlighting": true
          |}
          |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.didSave("a/src/main/scala/foo/Main.worksheet.sc")
      _ <- server.assertSemanticHighlight(
        "a/src/main/scala/foo/Main.worksheet.sc",
        expected,
        fileContent,
      )
    } yield ()
  }
}
