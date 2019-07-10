package tests.pc
import tests.BaseCompletionSuite

object SbtCompletionSuite extends BaseCompletionSuite {

  check(
    "interpolator",
    """|val myName = ""
       |myNam@@
       |""".stripMargin,
    """|myName: String
       |""".stripMargin,
    filename = "build.sbt",
    enablePackageWrap = false
  )
}
