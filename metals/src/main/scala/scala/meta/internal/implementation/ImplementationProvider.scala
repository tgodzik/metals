package scala.meta.internal.implementation

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentPositionParams
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.mtags.{Symbol => MSymbol}
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath
import java.nio.file.Paths
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.ClassSignature
import scala.meta.internal.semanticdb.TypeRef
import scala.meta.internal.semanticdb.Signature
import scala.meta.internal.semanticdb.TextDocument
import java.util.concurrent.ConcurrentHashMap
import java.nio.file.Path
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.MethodSignature
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.TokenEditDistance
import scala.meta.internal.semanticdb.TypeSignature
import scala.meta.internal.semanticdb.Scala._

final class ImplementationProvider(
    semanticdbs: Semanticdbs,
    workspace: AbsolutePath,
    index: GlobalSymbolIndex,
    buildTargets: BuildTargets,
    buffer: Buffers,
    definitionProvider: DefinitionProvider
) {
  import ImplementationProvider._

  private val globalTable = new GlobalClassTable(buildTargets)
  private val implementationsInPath =
    new ConcurrentHashMap[Path, Map[String, Set[ClassLocation]]]

  def clear(): Unit = {
    implementationsInPath.clear()
  }

  def onDelete(path: Path): Unit = {
    implementationsInPath.remove(path)
  }

  def onChange(docs: TextDocuments, path: Path): Unit = {
    implementationsInPath.compute(
      path, { (_, _) =>
        computeInheritance(docs)
      }
    )
  }

  def implementations(params: TextDocumentPositionParams): List[Location] = {
    val source = params.getTextDocument.getUri.toAbsolutePath

    def findSemanticDbForSymbol(symbol: String): Option[TextDocument] = {
      for {
        symbolDefinition <- index.definition(MSymbol(symbol))
        document <- findSemanticdb(symbolDefinition.path)
      } yield {
        document
      }
    }

    val locations = for {
      currentDoc <- findSemanticdb(source).toList
      positionOccurrence = definitionProvider.positionOccurrence(
        source,
        params,
        currentDoc
      )
      occ <- positionOccurrence.occurrence.toList
    } yield {
      val definitionDocument = if (currentDoc.definesSymbol(occ.symbol)) {
        Some(currentDoc)
      } else {
        findSemanticDbForSymbol(occ.symbol)
      }
      definitionDocument match {
        case None =>
          findForNonWorkspaceSymbol(occ, source)
          Nil
        case Some(textDocument) =>
          findForWorkspaceSymbol(textDocument, occ).toList
      }
    }
    locations.flatten.toList
  }

  def findForNonWorkspaceSymbol(
      occ: SymbolOccurrence,
      source: AbsolutePath
  ): Iterable[Location] = {

    val index = globalTable.indexFor(source) orElse {
      val allParentClasses = implementationsInPath.asScala.flatMap {
        case (path, map) => map.keySet
      }.toSet
      globalTable.indexFor(source, allParentClasses)
    }
    for {
      classesIndex <- index.toList
      _ = pprint.log(classesIndex.inheritance)
      plainSym <- classesIndex.symtab.info(occ.symbol).toList
      sym = plainSym.copy(
        signature =
          enrichSignature(plainSym.signature, classesIndex.symtab.info _)
      )
      classSym <- classFromSymbol(sym, classesIndex.symtab.info(_)).toIterable
      (file, locations) <- findImplementation(
        classSym.symbol,
        classesIndex.inheritance
      ).groupBy(_.file)
      realFile <- file.toIterable
      fileSource = AbsolutePath(realFile)
      doc <- findSemanticdb(fileSource).toIterable
      distance = TokenEditDistance.fromBuffer(fileSource, doc.text, buffer)
      impl <- locations
      implReal = impl.toRealNames(classSym, translateKey = true)
      implOccurence <- if (isClassLike(sym))
        findDefOccurence(doc, impl.symbol)
      else MethodImplementation.find(sym, classSym, implReal, doc)
      range <- implOccurence.range
      revised <- distance.toRevised(range.toLSP)
      uri = realFile.toUri.toString
    } yield new Location(uri, revised)
  }

  def findForWorkspaceSymbol(
      definitionDocument: TextDocument,
      occ: SymbolOccurrence
  ): Iterable[Location] = {
    for {
      plainSym <- findSymbol(definitionDocument, occ.symbol).toIterable
      sym = plainSym.copy(
        signature = enrichSignature(
          plainSym.signature,
          findSymbol(definitionDocument, _)
        )
      )
      classSym <- classFromSymbol(sym, findSymbol(definitionDocument, _))
        .toIterable
      (file, locations) <- findImplementation(classSym.symbol).groupBy(_.file)
      realFile <- file.toIterable
      fileSource = AbsolutePath(realFile)
      doc <- findSemanticdb(fileSource).toIterable
      distance = TokenEditDistance.fromBuffer(fileSource, doc.text, buffer)
      impl <- locations
      implReal = impl.toRealNames(classSym, translateKey = true)
      implOccurence <- if (isClassLike(sym))
        findDefOccurence(doc, impl.symbol)
      else MethodImplementation.find(sym, classSym, implReal, doc)
      range <- implOccurence.range
      revised <- distance.toRevised(range.toLSP)
      uri = realFile.toUri.toString
    } yield new Location(uri, revised)
  }

  private def findSemanticdb(fileSource: AbsolutePath): Option[TextDocument] =
    semanticdbs
      .textDocument(fileSource)
      .documentIncludingStale

  private def findImplementation(
      symbol: String,
      additional: Map[String, Set[ClassLocation]] = Map.empty
  ): Set[ClassLocation] = {
    val workspaceImpl = for {
      (_, symbols) <- implementationsInPath.asScala
      symbolImpls <- symbols.get(symbol).toList
      impl <- symbolImpls
    } yield impl
    val directImpl = workspaceImpl ++ additional.getOrElse(symbol, Set.empty)
    directImpl.toSet ++ directImpl
      .flatMap { loc =>
        findImplementation(loc.symbol, additional)
          .map(_.translateAsSeenFrom(loc))
      }
  }

  private def computeInheritance(
      docs: TextDocuments
  ): Map[String, Set[ClassLocation]] = {
    val allParents = for {
      doc <- docs.documents
      thisSymbol <- doc.symbols
      parent <- parentsFromSignature(
        thisSymbol.symbol,
        thisSymbol.signature,
        Some(doc),
        Some(workspace),
        findSymbol(doc, _)
      ).toList
    } yield parent

    allParents.groupBy(_._1).map {
      case (symbol, locations) =>
        symbol -> locations.map(_._2).toSet
    }
  }
}

object ImplementationProvider {

  def parentsFromSignature(
      symbol: String,
      signature: Signature,
      textDocument: Option[TextDocument],
      workspace: Option[AbsolutePath],
      findSymbol: String => Option[SymbolInformation]
  ): Seq[(String, ClassLocation)] = {
    val filePath = for {
      doc <- textDocument
      workspacePath <- workspace
    } yield workspacePath.toNIO.resolve(Paths.get(doc.uri))

    signature match {
      case classSig: ClassSignature =>
        val allLocations = classSig.parents.collect {
          case t: TypeRef =>
            val loc =
              ClassLocation(symbol, filePath, t, classSig)
            t.symbol -> loc
        }
        allLocations
      case tpe: TypeSignature =>
        tpe.lowerBound match {
          case tr: TypeRef =>
            findSymbol(tr.symbol).toSeq.flatMap {
              case info =>
                parentsFromSignature(
                  tr.symbol,
                  info.signature,
                  textDocument,
                  workspace,
                  findSymbol
                )
            }
          case _ =>
            Seq.empty
        }
      case _ =>
        Seq.empty
    }
  }

  def classFromSymbol(
      info: SymbolInformation,
      findSymbol: String => Option[SymbolInformation]
  ): Option[SymbolInformation] = {
    if (isClassLike(info)) {
      Some(info)
    } else {
      findSymbol(info.symbol.owner).filter(isClassLike)
    }
  }

  // TODO see how to deal with aliases with type parameters
  // TODO might not be needed
  def dealiasSymbol(
      info: SymbolInformation,
      findSymbol: String => Option[SymbolInformation]
  ): Option[SymbolInformation] = {
    info.signature match {
      case ts: TypeSignature =>
        val sym = ts.lowerBound.asInstanceOf[TypeRef].symbol
        findSymbol(sym)
      case _ =>
        Some(info)
    }
  }

  def findDefOccurence(
      semanticDb: TextDocument,
      symbol: String
  ): Option[SymbolOccurrence] = {
    semanticDb.occurrences.find(
      occ => occ.role.isDefinition && occ.symbol == symbol
    )
  }

  def findSymbol(
      semanticDb: TextDocument,
      symbol: String
  ): Option[SymbolInformation] = {
    semanticDb.symbols.find(
      sym => sym.symbol == symbol
    )
  }

  def isClassLike(info: SymbolInformation) =
    info.isObject || info.isClass || info.isTrait || info.isType

  def enrichSignature(
      signature: Signature,
      findSymbol: String => Option[SymbolInformation]
  ): Signature = {
    signature match {
      case methodSignature: MethodSignature =>
        enrichSignature(methodSignature, findSymbol)
      case _ => signature
    }
  }

  def enrichSignature(
      signature: MethodSignature,
      findSymbol: String => Option[SymbolInformation]
  ): MethodSignature = {
    val allParams = signature.parameterLists.map { scope =>
      val hardlinks = scope.symlinks.flatMap { sym =>
        findSymbol(sym)
      }
      scope.copy(hardlinks = hardlinks)
    }
    signature.copy(parameterLists = allParams)
  }
}
