package tests.mbt

import scala.concurrent.ExecutionContext

import scala.meta.internal.metals.Configs
import scala.meta.internal.metals.debug.TestFrameworkSymbolRegistry
import scala.meta.internal.metals.mbt.IndexingStats
import scala.meta.internal.metals.mbt.MbtWorkspaceSymbolProvider

import munit.AnyFixture
import tests.CustomLoggingFixture
import tests.FileLayout
import tests.TemporaryDirectoryFixture

class MbtTestCandidateCacheSuite extends munit.FunSuite {
  val workspace = new TemporaryDirectoryFixture()
  override def munitFixtures: Seq[AnyFixture[_]] =
    List(
      workspace,
      CustomLoggingFixture.showWarnings(),
    )

  override def munitExecutionContext: ExecutionContext = ExecutionContext.global

  def newProvider(): MbtWorkspaceSymbolProvider =
    new MbtWorkspaceSymbolProvider(
      workspace(),
      config = () => Configs.WorkspaceSymbolProviderConfig.mbt,
    )(munitExecutionContext)

  private def candidateSymbols(
      provider: MbtWorkspaceSymbolProvider
  ): Set[String] =
    provider
      .candidateTestClasses(
        filterPath = _ => true,
        annotationSymbols = TestFrameworkSymbolRegistry.annotationSymbols,
        baseParentSymbols = TestFrameworkSymbolRegistry.baseParentSymbols,
      )
      .map(_.candidateSymbol)
      .toSet

  private def writeLayout(): Unit = {
    FileLayout.fromString(
      """
/example/BaseSuite.scala
package example
trait BaseSuite extends munit.FunSuite
/example/HelloSuite.scala
package example
class HelloSuite extends munit.FunSuite {
  test("hello") {
    assert(true)
  }
}
/example/CustomSuite.scala
package example
class CustomSuite extends BaseSuite {
  test("custom") {
    assert(true)
  }
}
/example/NotATest.scala
package example
class NotATest {
  def hello(): Unit = ()
}
""",
      root = workspace(),
    )
  }

  test("discovers-direct-and-transitive-test-candidates") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()

    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 4),
    )

    val symbols = candidateSymbols(provider)
    assert(clue(symbols).contains("example/HelloSuite#"))
    assert(clue(symbols).contains("example/CustomSuite#"))
    // Traits are not runnable suite candidates.
    assert(!clue(symbols).contains("example/BaseSuite#"))
    assert(!clue(symbols).contains("example/NotATest#"))
    assertEquals(candidateSymbols(provider), symbols)
  }

  test("persists-candidates-across-restarts") {
    writeLayout()
    val provider1 = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider1.onReindex().awaitBackgroundJobs()
    val symbols1 = candidateSymbols(provider1)

    val provider2 = newProvider()
    val symbols2 = candidateSymbols(provider2)
    assertEquals(symbols2, symbols1)
    assertEquals(
      provider2.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 0),
    )
    assertEquals(candidateSymbols(provider2), symbols1)
  }

  test("reindexes-changed-file-and-keeps-cached-files") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()

    FileLayout.fromString(
      """
/example/HelloSuite.scala
package example
class HelloSuite extends munit.FunSuite {
  test("hello") {
    assert(true)
  }
  test("hello2") {
    assert(true)
  }
}
""",
      root = workspace(),
    )
    workspace.gitCommitAllChanges()
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 1),
    )
    val symbols = candidateSymbols(provider)
    assert(clue(symbols).contains("example/HelloSuite#"))
    assert(clue(symbols).contains("example/CustomSuite#"))
  }

  test("new-custom-base-suite-is-discovered") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()

    FileLayout.fromString(
      """
/example/AnotherBase.scala
package example
trait AnotherBase extends BaseSuite
/example/AnotherSuite.scala
package example
class AnotherSuite extends AnotherBase {
  test("another") {
    assert(true)
  }
}
""",
      root = workspace(),
    )
    workspace.gitCommitAllChanges()
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 6, updatedFiles = 2),
    )
    val symbols = candidateSymbols(provider)
    assert(!clue(symbols).contains("example/AnotherBase#"))
    assert(clue(symbols).contains("example/AnotherSuite#"))
    assert(clue(symbols).contains("example/CustomSuite#"))
  }
}
