package scala.meta.internal.metals

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import com.google.common.hash.BloomFilter
import com.google.common.hash.Funnels
import org.eclipse.lsp4j.ReferenceParams
import org.eclipse.lsp4j.Location
import scala.collection.concurrent.TrieMap
import scala.meta.internal.implementation.ImplementationProvider
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.DefinitionAlternatives.GlobalSymbol
import scala.meta.internal.mtags.SemanticdbClasspath
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.Synthetic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.metals.FilePosition.locationToFilePosition
import scala.meta.io.AbsolutePath
import scala.util.control.NonFatal

final class ReferenceProvider(
    workspace: AbsolutePath,
    semanticdbs: Semanticdbs,
    buffers: Buffers,
    definition: DefinitionProvider,
    implementation: ImplementationProvider
) {
  var referencedPackages: BloomFilter[CharSequence] = BloomFilters.create(10000)
  val index: TrieMap[Path, BloomFilter[CharSequence]] =
    TrieMap.empty[Path, BloomFilter[CharSequence]]

  def reset(): Unit = {
    index.clear()
  }
  def onDelete(file: Path): Unit = {
    index.remove(file)
  }

  def onChange(docs: TextDocuments, file: Path): Unit = {
    val count = docs.documents.foldLeft(0)(_ + _.occurrences.length)
    val syntheticsCount = docs.documents.foldLeft(0)(_ + _.synthetics.length)
    val bloom = BloomFilter.create(
      Funnels.stringFunnel(StandardCharsets.UTF_8),
      Integer.valueOf((count + syntheticsCount) * 2),
      0.01
    )
    index(file) = bloom
    docs.documents.foreach { d =>
      d.occurrences.foreach { o =>
        if (o.symbol.endsWith("/")) {
          referencedPackages.put(o.symbol)
        }
        bloom.put(o.symbol)
      }
      d.synthetics.foreach { synthetic =>
        Synthetics.foreachSymbol(synthetic) { sym =>
          bloom.put(sym)
          Synthetics.Continue
        }
      }
    }
    resizeReferencedPackages()
  }

  def references(params: ReferenceParams): ReferencesResult = {
    references(
      FilePosition(
        params.getTextDocument.getUri.toAbsolutePath,
        params.getPosition
      ),
      params.getContext.isIncludeDeclaration
    )
  }

  def references(
      filePosition: FilePosition,
      includeDeclaration: Boolean
  ): ReferencesResult = {
    semanticdbs
      .textDocument(filePosition.filePath)
      .documentIncludingStale match {
      case Some(doc) =>
        val ResolvedSymbolOccurrence(distance, maybeOccurrence) =
          definition.positionOccurrence(
            filePosition,
            doc
          )
        maybeOccurrence match {
          case Some(occurrence) =>
            val symbolName = occurrence.symbol.desc.name.value
            val shouldNotIncludeInheritance =
              ReferenceProvider.methodsSearchedWithoutInheritance.contains(
                symbolName
              )

            if (shouldNotIncludeInheritance) {
              currentSymbolReferences(
                filePosition,
                includeDeclaration
              )
            } else {
              ReferencesResult(
                occurrence.symbol,
                allInheritanceReferences(
                  occurrence,
                  doc,
                  filePosition,
                  failWhenReachingDependencySymbol = false,
                  fnIncludeSynthetics = _ => true
                )
              )
            }
          case None =>
            ReferencesResult.empty
        }
      case None =>
        ReferencesResult.empty
    }
  }

  def allInheritanceReferences(
      symbolOccurrence: SymbolOccurrence,
      doc: TextDocument,
      filePosition: FilePosition,
      fnIncludeSynthetics: Synthetic => Boolean,
      failWhenReachingDependencySymbol: Boolean,
      canSkipExactMatchCheck: Boolean = true
  ): Seq[Location] = {
    val parentSymbols = implementation
      .topMethodParents(doc, symbolOccurrence.symbol)

    val parentSymbolInDependency = parentSymbols.exists(_.isRight)
    if (failWhenReachingDependencySymbol && parentSymbolInDependency) {
      Seq.empty
    } else {

      val mainDefinitions = {
        if (parentSymbols.isEmpty) List(Left(filePosition))
        else parentSymbols.map(ps => ps.left.map(locationToFilePosition))
      }

      val topParentWorkspaceLocations =
        mainDefinitions.flatMap(_.swap.toOption)

      val isLocal = symbolOccurrence.symbol.isLocal
      val currentReferences = topParentWorkspaceLocations
        .flatMap(
          currentSymbolReferences(
            _,
            includeDeclaration = isLocal,
            canSkipExactMatchCheck = canSkipExactMatchCheck,
            includeSynthetics = fnIncludeSynthetics
          ).locations
        )
      val definitionLocation = {
        val parentSymbolLocs = parentSymbols.flatMap(_.left.toOption)
        if (parentSymbolLocs.isEmpty)
          definition
            .fromSymbol(symbolOccurrence.symbol)
            .asScala
            .filter(_.getUri.isScalaFilename)
        else parentSymbolLocs
      }
      val implReferences = mainDefinitions.flatMap(_ =>
        implementations(
          // TODO
          "",
          filePosition.filePath,
          !symbolOccurrence.symbol.desc.isType,
          canSkipExactMatchCheck
        )
      )

      currentReferences ++ implReferences ++ definitionLocation
    }
  }

  private def implementations(
      symbol: String,
      source: AbsolutePath,
      localDocument: Option[TextDocument],
      localDistance: Option[TokenEditDistance],
      shouldCheckImplementation: Boolean,
      canSkipExactMatchCheck: Boolean
  ): Seq[Location] = {
    if (shouldCheckImplementation) {
      for {
        implLoc <- implementation.implementations(
          currentDocument = None,
          symbol,
          source
        )
        loc <- currentSymbolReferences(
          "", // TODO we need to return string instead of location
          localDocument,
          localDistance,
          includeDeclaration = true,
          canSkipExactMatchCheck = canSkipExactMatchCheck
        ).locations
      } yield loc
    } else {
      Nil
    }
  }

  def currentSymbolReferences(
      symbol: String,
      localDocument: Option[TextDocument],
      localDistance: Option[TokenEditDistance],
      includeDeclaration: Boolean,
      canSkipExactMatchCheck: Boolean = true,
      includeSynthetics: Synthetic => Boolean = _ => true
  ): ReferencesResult = {
    if (symbol.isLocal) {
      (localDocument, localDistance) match {
        case (Some(doc), Some(distance)) =>
          val locations = referenceLocations(
            doc,
            Set.empty,
            distance,
            workspace.resolve(doc.uri).toURI.toString(),
            includeDeclaration,
            canSkipExactMatchCheck,
            includeSynthetics
          )
          ReferencesResult(symbol, locations)
        case _ =>
          ReferencesResult.empty
      }
    } else {

      val locations = currentSymbolReferences(
        symbol,
        includeDeclaration,
        canSkipExactMatchCheck,
        includeSynthetics
      )
      ReferencesResult(symbol, locations)
    }
  }

  private def currentSymbolReferences(
      symbol: String,
      isIncludeDeclaration: Boolean,
      canSkipExactMatchCheck: Boolean,
      includeSynthetics: Synthetic => Boolean
  ): Seq[Location] = {
    val isSymbol = alternatives + symbol
    val visited = scala.collection.mutable.Set.empty[AbsolutePath]
    val results: Iterator[Location] = for {
      (path, bloom) <- index.iterator
      if bloom.mightContain(symbol)
      scalaPath <- SemanticdbClasspath
        .toScala(workspace, AbsolutePath(path))
        .iterator
      if !visited(scalaPath)
      _ = visited.add(scalaPath)
      if scalaPath.exists
      semanticdb <- semanticdbs
        .textDocument(scalaPath)
        .documentIncludingStale
        .iterator
      semanticdbDistance = TokenEditDistance.fromBuffer(
        scalaPath,
        semanticdb.text,
        buffers
      )
      uri = scalaPath.toURI.toString
      reference <- try {
        referenceLocations(
          semanticdb,
          isSymbol,
          semanticdbDistance,
          uri,
          isIncludeDeclaration,
          canSkipExactMatchCheck,
          includeSynthetics
        )
      } catch {
        case NonFatal(e) =>
          // Can happen for example if the SemanticDB text is empty for some reason.
          scribe.error(s"reference: $scalaPath", e)
          Nil
      }
    } yield reference
    results.toSeq

  }

  private def referenceLocations(
      snapshot: TextDocument,
      isSymbol: Set[String],
      distance: TokenEditDistance,
      uri: String,
      isIncludeDeclaration: Boolean,
      canSkipExactMatchCheck: Boolean,
      includeSynthetics: Synthetic => Boolean
  ): Seq[Location] = {
    val buf = Seq.newBuilder[Location]
    def add(range: s.Range): Unit = {
      val revised = distance.toRevised(range.startLine, range.startCharacter)
      val dirtyLocation = range.toLocation(uri)
      for {
        location <- revised.toLocation(dirtyLocation)
      } {
        buf += location
      }
    }

    for {
      reference <- snapshot.occurrences
      if isSymbol(reference.symbol)
      if !reference.role.isDefinition || isIncludeDeclaration
      range <- reference.range.toList
    } {

      /* We skip checking if the symbol name matches exactly
       * in case of finding references, where false positives
       * are ok and speed is more important. This was needed
       * for some issues with macro annotations, so with renames we
       * must be sure that a proper name is replaced.
       */
      if (canSkipExactMatchCheck) {
        add(range)
      } else {
        findRealRange(range, snapshot.text, reference.symbol).foreach(add)
      }
    }

    for {
      synthetic <- snapshot.synthetics
      if Synthetics.existsSymbol(synthetic)(isSymbol) && includeSynthetics(
        synthetic
      )
      range <- synthetic.range.toList
    } add(range)

    buf.result()
  }

  private def findRealRange(
      range: s.Range,
      text: String,
      symbol: String
  ): Option[s.Range] = {
    val name = findName(range, text)
    val isBackticked = name.charAt(0) == '`'
    val realName =
      if (isBackticked) name.substring(1, name.length() - 1)
      else name
    if (symbol.isLocal || symbol.contains(realName)) {
      val realRange = if (isBackticked) {
        range
          .withStartCharacter(range.startCharacter + 1)
          .withEndCharacter(range.endCharacter - 1)
      } else {
        range
      }
      Some(realRange)
    } else {
      None
    }
  }

  private def findName(range: s.Range, text: String): String = {
    var i = 0
    var max = 0
    while (max < range.startLine) {
      if (text.charAt(i) == '\n') max += 1
      i += 1
    }
    val start = i + range.startCharacter
    val end = i + range.endCharacter
    text.substring(start, end)
  }

  private def resizeReferencedPackages(): Unit = {
    // Increase the size of the set of referenced packages if the false positive ratio is too high.
    if (referencedPackages.expectedFpp() > 0.05) {
      referencedPackages =
        BloomFilters.create(referencedPackages.approximateElementCount() * 2)
    }
  }

}

object ReferenceProvider {
  val methodsSearchedWithoutInheritance: Set[String] = Set("eq", "equals",
    "hashCode", "toString", "clone", "notify", "wait", "getClass")
}
