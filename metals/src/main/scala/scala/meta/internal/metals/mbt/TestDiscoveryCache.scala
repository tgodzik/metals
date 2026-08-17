package scala.meta.internal.metals.mbt

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import scala.meta.internal.jmbt.Mbt
import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.FingerprintedCharSequence
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.debug.TestFrameworkSymbolRegistry
import scala.meta.internal.mtags.Symbol

/**
 * Pure helpers for the OID-keyed MBT test/main candidate cache.
 *
 * The expensive workspace-wide BFS is replaced by:
 *   1. a global test-base closure of framework + custom base-suite symbols
 *   2. per-document [[TestDiscoveryData]] derived from a document's bloom filter
 *      against that closure
 */
object TestDiscoveryCache {

  /** Digest of the current framework registry; invalidates persisted closures. */
  def currentRegistryDigest: String = {
    val symbols =
      (TestFrameworkSymbolRegistry.annotationSymbols ++
        TestFrameworkSymbolRegistry.baseParentSymbols).sorted
    sha1(symbols.mkString("\n"))
  }

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

  /** Top-level class/trait symbols defined in the document (closure expansion). */
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
  private val mainAnnotFingerprint: CharSequence =
    FingerprintedCharSequence.fuzzyReference("scala/main#")
  private val appFingerprint: CharSequence =
    FingerprintedCharSequence.fuzzyReference("scala/App#")

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
    // Match how possibleReferences queries App/@main: display-name fingerprints
    // (App., App:, ...), not the full-symbol fingerprint alone.
    val matchesMainRefs =
      documentMatchesFingerprints(doc, mainReferenceFingerprints) ||
        doc.bloomFilter.mightContain(mainAnnotFingerprint) ||
        doc.bloomFilter.mightContain(appFingerprint)
    if (matchesMainRefs) {
      val matchesMainAnnot =
        doc.bloomFilter.mightContain(mainAnnotFingerprint) ||
          documentMatchesFingerprints(
            doc,
            fingerprintsFor(
              references = Seq("scala/main#"),
              implementations = Nil,
            ),
          )
      if (matchesMainAnnot) {
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
      closureFingerprints: Seq[CharSequence],
  ): TestDiscoveryData = {
    val matches = documentMatchesFingerprints(doc, closureFingerprints)
    val testCandidates =
      if (matches) toplevelClassOrObjectSymbols(doc) else Seq.empty
    TestDiscoveryData(
      matchesTestClosure = matches,
      testCandidateSymbols = testCandidates,
      mainCandidateSymbols = computeMainCandidateSymbols(doc),
    )
  }

  def toProto(
      registryDigest: String,
      baseSymbols: Iterable[String],
  ): Mbt.TestDiscoveryIndex =
    Mbt.TestDiscoveryIndex
      .newBuilder()
      .setRegistryDigest(registryDigest)
      .addAllBaseSymbol(baseSymbols.asJava)
      .build()

  private def sha1(text: String): String = {
    val digest = MessageDigest.getInstance("SHA-1")
    digest
      .digest(text.getBytes(StandardCharsets.UTF_8))
      .map("%02x".format(_))
      .mkString
  }
}
