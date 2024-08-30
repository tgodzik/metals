package tests.info

import munit._
import tests.BasePCSuite
import scala.meta.internal.metals.CompilerOffsetParams
import java.nio.file.Paths
import scala.meta.internal.mtags.CommonMtagsEnrichments.XtensionOptionalJava
import scala.meta._

class InformationSuite extends BasePCSuite {

  def check(
      testOpt: TestOptions,
      original: String,
      symbol: String,
      expected: String,
      automaticPackage: Boolean = true,
      compat: Map[String, String] = Map.empty
  )(implicit loc: Location): Unit = {
    test(testOpt) {
      val filename = "Info.scala"
      val pkg = scala.meta.Term.Name(testOpt.name).syntax
      val noRange = original
        .replace("<<", "")
        .replace(">>", "")
      val packagePrefix =
        if (automaticPackage) s"package $pkg\n"
        else ""
      val codeOriginal = packagePrefix + noRange
      val (code, so, _) = hoverParams(codeOriginal, filename)
      val pcParams =
        CompilerOffsetParams(Paths.get(filename).toUri(), code, so)
      val _ = presentationCompiler
        .hover(pcParams)
        .get()
        .asScala
        .map(_.toLsp())

      val obtained = presentationCompiler
        .info(symbol)
        .get()
        .asScala
        .map(_.toString())
        .getOrElse("")
      assertNoDiff(
        obtained,
        getExpected(expected, compat, scalaVersion)
      )
    }
  }

  check(
    "main",
    """|package examples
       |
       |@main def <<ma@@inMain>> = ???
       |""".stripMargin,
    "examples/mainMain#",
    ""
  )
}
