package scala.meta.internal.metals.mbt

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.FingerprintedCharSequence
import scala.meta.internal.metals.debug.TestFrameworkSymbolRegistry
import scala.meta.internal.mtags.Symbol

/**
 * Helpers for attaching test/main class candidates to a single indexed
 * document. Discovery data lives on the document and is dropped whenever
 * the file is re-indexed (new OID).
 */
object TestDiscoveryCache {

  def annotationSymbols: Seq[String] =
    TestFrameworkSymbolRegistry.annotationSymbols

  def seedBaseParentSymbols: Seq[String] =
    TestFrameworkSymbolRegistry.baseParentSymbols

  /**
   * Builds the fuzzy-reference fingerprints that
   * [[MbtWorkspaceSymbolProvider.possibleReferences]] would use for the given
   * reference and implementation symbol sets.
   */
  def fingerprintsFor(
      references: collection.Seq[String],
      implementations: collection.Seq[String],
  ): Seq[CharSequence] = {
    val queries = HashSet.empty[String]
    implementations.foreach { symbol =>
      val sym = Symbol(symbol)
      if (sym.isMethod) {
        queries += s"${sym.displayName}():"
      } else if (sym.isType) {
        queries += s"${sym.displayName}:"
        // Scala top-level mtags indexer does not always emit ':'
        queries += s"${sym.displayName}#"
      } else if (sym.isTerm) {
        queries += s"${sym.displayName}."
        queries += s"${sym.displayName}():"
      }
    }
    references.foreach { ref =>
      val sym = Symbol(ref)
      if (sym.isGlobal) {
        if (sym.isConstructor) {
          queries += s"${sym.owner.displayName}."
        } else if (sym.isMethod) {
          queries += s"${sym.displayName}()."
        } else {
          queries += s"${sym.displayName}."
          queries += s"${sym.displayName}:"
          queries += s"${sym.displayName}()."
        }
      }
    }
    queries.iterator.map(FingerprintedCharSequence.fuzzyReference(_)).toSeq
  }

  def documentMatchesFingerprints(
      doc: IndexedDocument,
      fingerprints: Seq[CharSequence],
  ): Boolean =
    fingerprints.exists(query => doc.bloomFilter.mightContain(query))

  /** Top-level class/trait symbols defined in the document. */
  def toplevelTypeSymbols(doc: IndexedDocument): Seq[String] = {
    val result = ArrayBuffer.empty[String]
    for (symbolInfo <- doc.symbols) {
      val kind = symbolInfo.getKind
      if (
        (kind == Semanticdb.SymbolInformation.Kind.TRAIT ||
          kind == Semanticdb.SymbolInformation.Kind.CLASS) &&
        Symbol(symbolInfo.getSymbol).isToplevel
      ) {
        result += symbolInfo.getSymbol
      }
    }
    result.toSeq
  }

  /** Top-level class/object symbols that can be test-suite candidates. */
  def toplevelClassOrObjectSymbols(doc: IndexedDocument): Seq[String] = {
    val result = ArrayBuffer.empty[String]
    for (symbolInfo <- doc.symbols) {
      val kind = symbolInfo.getKind
      if (
        (kind == Semanticdb.SymbolInformation.Kind.CLASS ||
          kind == Semanticdb.SymbolInformation.Kind.OBJECT) &&
        Symbol(symbolInfo.getSymbol).isToplevel
      ) {
        result += symbolInfo.getSymbol
      }
    }
    result.toSeq
  }

  private val javaMainSuffix = "#main()."
  private val scalaMainSuffix = ".main()."
  // Same fingerprints possibleReferences uses for scala/main# and scala/App#.
  private val mainReferenceFingerprints: Seq[CharSequence] =
    fingerprintsFor(
      references = Seq("scala/main#", "scala/App#"),
      implementations = Nil,
    )
  private val mainAnnotFingerprints: Seq[CharSequence] =
    fingerprintsFor(
      references = Seq("scala/main#"),
      implementations = Nil,
    )

  /**
   * Derives main-class candidate symbols from a single document without a
   * workspace-wide scan.
   */
  def computeMainCandidateSymbols(doc: IndexedDocument): Seq[String] = {
    val result = mutable.LinkedHashSet.empty[String]
    for (symbolInfo <- doc.symbols) {
      val symbol = symbolInfo.getSymbol
      if (symbol.endsWith(javaMainSuffix) || symbol.endsWith(scalaMainSuffix)) {
        result += symbol.stripSuffix("main().")
      }
    }
    val matchesMainRefs =
      documentMatchesFingerprints(doc, mainReferenceFingerprints)
    if (matchesMainRefs) {
      if (documentMatchesFingerprints(doc, mainAnnotFingerprints)) {
        for (symbolInfo <- doc.symbols) {
          if (symbolInfo.getKind == Semanticdb.SymbolInformation.Kind.METHOD) {
            result += symbolInfo.getSymbol
          }
        }
      }
      for (symbolInfo <- doc.symbols) {
        val symbol = symbolInfo.getSymbol
        if (
          symbolInfo.getKind == Semanticdb.SymbolInformation.Kind.OBJECT &&
          Symbol(symbol).isToplevel
        ) {
          result += symbol
        }
      }
    }
    result.toSeq
  }

  def computeTestDiscoveryData(
      doc: IndexedDocument,
      matchesTestFramework: Boolean,
  ): TestDiscoveryData = {
    val testCandidates =
      if (matchesTestFramework) toplevelClassOrObjectSymbols(doc)
      else Seq.empty
    TestDiscoveryData(
      matchesTestFramework = matchesTestFramework,
      testCandidateSymbols = testCandidates,
      mainCandidateSymbols = computeMainCandidateSymbols(doc),
    )
  }
}
