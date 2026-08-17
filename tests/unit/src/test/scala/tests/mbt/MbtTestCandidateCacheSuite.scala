package tests.mbt

import scala.concurrent.ExecutionContext

import scala.meta.internal.metals.Configs
import scala.meta.internal.metals.debug.TestFrameworkSymbolRegistry
import scala.meta.internal.metals.mbt.IndexingStats
import scala.meta.internal.metals.mbt.MbtWorkspaceSymbolProvider
import scala.meta.internal.metals.mbt.TestDiscoveryCache

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
    assertEquals(provider.testClosureFullScans, 1)

    val symbols = candidateSymbols(provider)
    assert(clue(symbols).contains("example/HelloSuite#"))
    assert(clue(symbols).contains("example/CustomSuite#"))
    // Traits are base types in the closure, not runnable suite candidates.
    assert(!clue(symbols).contains("example/BaseSuite#"))
    assert(!clue(symbols).contains("example/NotATest#"))
    // Second lookup must reuse the side index (no extra full scan).
    assertEquals(candidateSymbols(provider), symbols)
    assertEquals(provider.testClosureFullScans, 1)
  }

  test("persists-candidates-across-restarts-without-full-scan") {
    writeLayout()
    val provider1 = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider1.onReindex().awaitBackgroundJobs()
    val symbols1 = candidateSymbols(provider1)
    assertEquals(provider1.testClosureFullScans, 1)

    // New provider reads .metals/index.mbt including the persisted closure.
    val provider2 = newProvider()
    assertEquals(provider2.testClosureFullScans, 0)
    val symbols2 = candidateSymbols(provider2)
    assertEquals(symbols2, symbols1)
    assertEquals(provider2.testClosureFullScans, 0)
    assertEquals(
      provider2.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 0),
    )
    assertEquals(provider2.testClosureFullScans, 0)
    assertEquals(candidateSymbols(provider2), symbols1)
  }

  test("reindexes-changed-file-without-full-scan") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()
    assertEquals(provider.testClosureFullScans, 1)

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
    assertEquals(provider.testClosureFullScans, 1)
    assert(clue(candidateSymbols(provider)).contains("example/HelloSuite#"))
  }

  test("delta-scan-picks-up-new-custom-base-suite") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()
    assertEquals(provider.testClosureFullScans, 1)

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
    // Closure expansion uses a delta scan, not a full BFS.
    assertEquals(provider.testClosureFullScans, 1)
    val symbols = candidateSymbols(provider)
    // AnotherBase is a trait (closure member), AnotherSuite is the candidate.
    assert(!clue(symbols).contains("example/AnotherBase#"))
    assert(clue(symbols).contains("example/AnotherSuite#"))
  }

  test("registry-digest-mismatch-forces-full-recompute") {
    writeLayout()
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()
    assertEquals(provider.testClosureFullScans, 1)

    provider.invalidateTestDiscoveryCacheForTesting()
    val symbols = candidateSymbols(provider)
    assertEquals(provider.testClosureFullScans, 2)
    assert(clue(symbols).contains("example/HelloSuite#"))
    assert(clue(symbols).contains("example/CustomSuite#"))
  }

  test("registry-digest-is-stable") {
    val d1 = TestDiscoveryCache.currentRegistryDigest
    val d2 = TestDiscoveryCache.currentRegistryDigest
    assertEquals(d1, d2)
    assert(d1.nonEmpty)
  }
}
