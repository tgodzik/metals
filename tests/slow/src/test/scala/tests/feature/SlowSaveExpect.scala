package tests.feature

import tests.BaseExpectSuite

object SlowSaveExpect {
  def main(args: Array[String]): Unit = {
    List[BaseExpectSuite](
      new SemanticTokensScala3Suite(),
      new SemanticTokensScala3SpecificSuite(),
    ).foreach { suite =>
      val header = suite.suiteName.length + 2
      println("=" * header)
      println("= " + suite.suiteName)
      println("=" * header)
      suite.saveExpect()
      suite.afterAll()
    }
  }

}
