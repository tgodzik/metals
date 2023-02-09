package tests.feature

import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.MetalsEnrichments._
import tests.InputProperties
import tests.DirectoryExpectSuite
import tests.ExpectTestCase
import tests.TestSemanticTokens
import tests.TestMtagsResolver
import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.internal.metals.Embedded
import scala.meta.internal.metals.StatusBar
import scala.meta.internal.io.PathIO
import scala.meta.internal.metals.ProgressTicks
import tests.FakeTime
import tests.TestingClient
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.ClientConfiguration
import scala.meta.internal.metals.MtagsBinaries

abstract class BaseSemanticTokensSlowExpectSuite(
    input: InputProperties,
    suiteName: String,
) extends DirectoryExpectSuite(suiteName) {

  private val compiler = {
    val resolver = new TestMtagsResolver()
    resolver.resolve(V.scala3) match {

      case Some(mtags: MtagsBinaries.Artifacts) =>
        val time = new FakeTime
        val client = new TestingClient(PathIO.workingDirectory, Buffers())
        val status = new StatusBar(
          client,
          time,
          ProgressTicks.dots,
          ClientConfiguration.default,
        )(munitExecutionContext)
        val embedded = new Embedded(status)
        embedded
          .presentationCompiler(mtags, mtags.jars)
          .newInstance(
            "tokens",
            input.classpath.entries.map(_.toNIO).asJava,
            Nil.asJava,
          )
      case _ => fail(s"Could not load ${V.scala3} presentation compiler")
    }

  }

  override def testCases(): List[ExpectTestCase] = {
    input.scalaFiles.map { file =>
      ExpectTestCase(
        file,
        () => {
          val tokens = compiler
            .semanticTokens(
              CompilerVirtualFileParams(
                file.file.toURI,
                file.code,
                EmptyCancelToken,
              )
            )
            .get()
          TestSemanticTokens.semanticString(
            file.code,
            tokens.asScala.toList.map(_.toInt),
          )
        },
      )
    }
  }

  override def afterAll(): Unit = {
    compiler.shutdown()
  }
}

class SemanticTokensScala3SpecificSuite
    extends BaseSemanticTokensSlowExpectSuite(
      InputProperties.scala3(),
      "semanticTokens3",
    )

class SemanticTokensScala3Suite
    extends BaseSemanticTokensSlowExpectSuite(
      InputProperties.scala2(),
      "semanticTokens3Base",
    )
