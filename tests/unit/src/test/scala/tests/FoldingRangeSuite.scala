package tests

import java.nio.file.Paths
import java.util
import java.util.UUID
import org.eclipse.{lsp4j => l}
import tests.BuildInfo.testResourceDirectory
import scala.meta.internal.metals.FoldingRangeProvider
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.Trees
import scala.meta.internal.metals.MetalsEnrichments._

object FoldingRangeSuite extends DirectoryExpectSuite("foldingRange/expect") {
  private val trees = new Trees()

  private val foldingRangeProvider =
    new FoldingRangeProvider(trees, foldOnlyLines = false)

  override def testCases(): List[ExpectTestCase] = {
    val inputDirectory = AbsolutePath(testResourceDirectory)
      .resolve("foldingRange")
      .resolve("input")
    val customInput = InputProperties.fromDirectory(inputDirectory)
    customInput.allFiles.map { file =>
      ExpectTestCase(file, () => obtainFrom(file))
    }
  }

  private def obtainFrom(file: InputFile): String = {
    val scalaSource = file.input.text

    val actualRanges = findFoldingRangesFor(scalaSource)
    val edits = FoldingRangesTextEdits.apply(actualRanges)
    TextEdits.applyEdits(scalaSource, edits)
  }

  private def findFoldingRangesFor(
      source: String
  ): util.List[l.FoldingRange] = {
    val path = registerSource(source)
    foldingRangeProvider.getRangedFor(path.filename, source)
  }

  private def registerSource(source: String): AbsolutePath = {
    val name = UUID.randomUUID().toString + ".scala"
    val path = AbsolutePath(Paths.get(name))
    trees.didChange(path.filename, source)
    path
  }
}
