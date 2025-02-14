package scala.meta.internal.query

import scala.meta.internal.metals.WorkspaceSymbolProvider
import scala.meta.pc.SymbolSearchVisitor
import java.nio.file.Path
import org.eclipse.lsp4j
import org.eclipse.lsp4j.SymbolKind
import scala.collection.mutable.ListBuffer
import scala.meta.internal.metals.WorkspaceSymbolQuery

/**
 * This is very basic, we would need to put some work into it to make it more useful
 *
 * This is based on WorkspaceSymbolSearchPRovider, which is the thing that underpins
 * the symbols search for LSP
 *
 * We also have IndexedSymbols class that is being used for tree view, but it's purpose is
 * to provide a tree view of the symbols, not to search for them.
 *
 * @param workspaceSearchProvider
 */
class QueryEngine(
    workspaceSearchProvider: WorkspaceSymbolProvider
) {
  def findPackage(
      packageName: String
  ): Set[String] = {
    val packages = ListBuffer.empty[String]

    // I think we might only index top level symbols such as classes
    val visitor = new SymbolSearchVisitor {
      override def shouldVisitPackage(pkg: String): Boolean = {
        if (pkg.contains(packageName)) {
          // we can capture packages this way, we don't notmally index them since they don't have a place for definition
          packages += pkg
          true
        } else {
          false
        }
      }

      /* dependencies, more advanced example in WorkspaceSearchVisitor
       * We usually construct semanticdb symbols from classfiles, then go to definition
       * to find the source file, which we can then easily index using ScalaMtags or JavaMtags
       *
       */
      override def visitClassfile(pkg: String, filename: String): Int = {

        0
      }

      // For anything in local workspace
      override def visitWorkspaceSymbol(
          path: Path,
          symbol: String,
          kind: SymbolKind,
          range: lsp4j.Range,
      ): Int = {

        pprint.log(symbol)
        0
      }

      override def isCancelled(): Boolean = false

    }
    // matches method can be modified to better suite the needs
    val query = WorkspaceSymbolQuery(
      packageName,
      WorkspaceSymbolQuery.AlternativeQuery.all(packageName),
      false,
      true,
    )
    val id = workspaceSearchProvider.buildTargets.allBuildTargetIds.headOption
    workspaceSearchProvider.search(
      query,
      visitor,
      id,
    )
    val set = packages.toList.toSet
    set
  }
}
