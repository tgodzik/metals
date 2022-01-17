package tests.hover

import java.nio.file.Paths

import scala.meta.internal.metals.CompilerOffsetParams
import munit.TestOptions
import tests.BasePCSuite
import tests.RangeReplace
import munit.Location
import scala.collection.JavaConverters._

class DocumentHighlightSuite extends BasePCSuite with RangeReplace {

  check(
    "single",
    """
      |object Main {
      |  Option(1).<<he@@ad>>
      |}""".stripMargin
  )

  check(
    "multiple",
    """
      |object Main {
      |  val <<abc>> = 123
      |  <<abc>>.toInt
      |  println(<<ab@@c>>)
      |}""".stripMargin
  )

  check(
    "different-symbols",
    """
      |object Main {
      |  val abc = 123
      |  abc.<<to@@Int>>
      |  134l.toInt
      |}""".stripMargin
  )

  check(
    "scopes",
    """
      |object Main {
      |  val <<@@a>> = 123
      |  val f = (a: Int) => a + 1
      |  println(<<a>>)
      |}""".stripMargin
  )

  check(
    "params",
    """
      |case class User(<<name>>: String)
      |object Main {
      |  val user = User(<<na@@me>> = "Susan")
      |  println(user.<<name>>)
      |  user.copy(<<name>> = "John")
      |}""".stripMargin
  )

  check(
    "object",
    """
      |case class <<User>>(name: String)
      |object <<User>>
      |object Main {
      |  val user = <<U@@ser>>(name = "Susan")
      |  println(user.name)
      |  user.copy(name = "John")
      |}""".stripMargin
  )

  check(
    "case-class-var",
    """
      |case class User(var <<name>>: String)
      |object Main {
      |  val user = User(<<na@@me>> = "Susan")
      |  println(user.<<name>>)
      |  user.<<name>> = ""
      |  user.copy(<<name>> = "John")
      |}""".stripMargin
  )

  check(
    "var",
    """
      |object Main {
      |  var <<abd>> = 123
      |  <<ab@@d>> = 344
      |  <<abd>> +=1
      |  println(<<abd>>)
      |}""".stripMargin
  )

  check(
    "overloaded".only,
    """
      |object Main {
      |  def hello() = ""
      |  def <<hel@@lo>>(a : Int) = ""
      |  def hello(a : Int, b : String) = ""
      |}""".stripMargin
  )

  def check(
      testOpt: TestOptions,
      original: String,
      compat: Map[String, String] = Map.empty
  )(implicit loc: Location): Unit = {
    test(testOpt) {
      val filename = "Hover.scala"
      val noRange = original
        .replace("<<", "")
        .replace(">>", "")
      val offset = noRange.indexOf("@@")
      val pcParams =
        CompilerOffsetParams(
          Paths.get(filename).toUri(),
          noRange.replace("@@", ""),
          offset
        )
      val highlights = presentationCompiler
        .documentHighlight(pcParams)
        .get()
        .asScala
        .sortBy(_.getRange().getStart().getLine())
      val obtained: String = highlights.foldLeft(noRange.replace("@@", "")) {
        case (txt, highlight) =>
          replaceInRange(txt, highlight.getRange())
      }
      assertNoDiff(
        obtained,
        original.replace("@@", "")
      )
    }
  }
}
