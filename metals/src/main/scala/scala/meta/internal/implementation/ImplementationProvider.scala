package scala.meta.internal.implementation

import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.TextDocumentPositionParams
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.mtags.{Symbol => MSymbol}
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath
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
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.TypeSignature
import scala.collection.mutable

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

      val context = definitionDocument match {
        case None =>
          globalTable.globalContextFor(
            source,
            implementationsInPath.asScala.toMap
          )
        case Some(textDocument) =>
          lazy val global = globalTable.globalSymbolTableFor(source)
          def symbolSearch(symbol: String) =
            findSymbol(textDocument, symbol)
              .orElse(findClassDef(symbol))
              .orElse(global.flatMap(_.info(symbol)))

          Some(
            InheritanceContext.fromDefinitions(
              symbolSearch,
              implementationsInPath.asScala.toMap
            )
          )
      }
      findLocations(occ.symbol, source, context)
    }
    locations.flatten.toList
  }

  def findLocations(
      symbol: String,
      source: AbsolutePath,
      context: Option[InheritanceContext]
  ): Iterable[Location] = {
    for {
      classContext <- context.toList
      plainSym <- classContext.findSymbol(symbol).toList
      sym = plainSym.copy(
        signature = enrichSignature(plainSym.signature, classContext.findSymbol)
      )
      classSym <- classFromSymbol(sym, classContext.findSymbol).toIterable
      (file, locations) <- findImplementation(classSym.symbol, classContext)
        .filter(_.file.isDefined)
        .groupBy(_.file)
      realFile <- file.toIterable
      fileSource = AbsolutePath(realFile)
      doc <- findSemanticdb(fileSource).toIterable
      distance = TokenEditDistance.fromBuffer(fileSource, doc.text, buffer)
      impl <- locations
      implReal = impl.toRealNames(classSym, translateKey = true)
      implSymbol <- if (isClassLike(sym))
        Some(impl.symbol)
      else {
        lazy val global = globalTable.globalSymbolTableFor(source)
        def localSearch(symbol: String): Option[SymbolInformation] = {
          findSymbol(doc, symbol)
            .orElse(findClassDef(symbol))
            .orElse(global.flatMap(_.info(symbol)))
        }
        MethodImplementation.find(
          sym,
          classSym,
          classContext,
          implReal,
          localSearch
        )
      }
      implOccurence <- findDefOccurence(doc, implSymbol)
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
      classContext: InheritanceContext
  ): Set[ClassLocation] = {
    val directImplementations = classContext.getLocations(symbol)
    directImplementations.toSet ++ directImplementations
      .flatMap { loc =>
        val allPossible = findImplementation(loc.symbol, classContext)
        allPossible.map(_.translateAsSeenFrom(loc))
      }
  }

  private def computeInheritance(
      docs: TextDocuments
  ): Map[String, Set[ClassLocation]] = {
    val allParents = new mutable.ListBuffer[(String, ClassLocation)]
    for {
      doc <- docs.documents
      thisSymbol <- doc.symbols

    } {
      if (isClassLike(thisSymbol)) {
        allParents ++= parentsFromSignature(
          thisSymbol.symbol,
          thisSymbol.signature,
          Some(workspace.resolve(doc.uri))
        )
      }
    }
    val mappedParents = allParents.groupBy(_._1).map {
      case (symbol, locations) =>
        symbol -> locations.map(_._2).toSet
    }
    mappedParents

  }

  private def findClassDef(symbol: String): Option[SymbolInformation] = {
    findSemanticDbForSymbol(symbol).flatMap(findSymbol(_, symbol))
  }

  private def findSemanticDbForSymbol(symbol: String): Option[TextDocument] = {
    for {
      symbolDefinition <- index.definition(MSymbol(symbol))
      document <- findSemanticdb(symbolDefinition.path)
    } yield {
      document
    }
  }

  private def classFromSymbol(
      info: SymbolInformation,
      findSymbol: String => Option[SymbolInformation]
  ): Option[SymbolInformation] = {
    val classInfo = if (isClassLike(info)) {
      Some(info)
    } else {
      findSymbol(info.symbol.owner)
        .filter(info => isClassLike(info))
    }
    classInfo.map(inf => dealiasClass(inf, findSymbol))
  }

}

object ImplementationProvider {

  // TODO maybe we don't have to look if we can know the symbol is not a type
  def dealiasClass(
      symbol: String,
      findSymbol: String => Option[SymbolInformation]
  ): String = {
    findSymbol(symbol)
      .map(inf => dealiasClass(inf, findSymbol))
      .map(_.symbol)
      .getOrElse(symbol)
  }

  def dealiasClass(
      info: SymbolInformation,
      findSymbol: String => Option[SymbolInformation]
  ): SymbolInformation = {
    if (info.isType) {
      info.signature match {
        case ts: TypeSignature =>
          ts.upperBound match {
            case tr: TypeRef =>
              findSymbol(tr.symbol)
                .getOrElse(info)
            case _ =>
              info
          }
        case _ => info
      }
    } else {
      info
    }
  }

  def findSymbol(
      semanticDb: TextDocument,
      symbol: String
  ): Option[SymbolInformation] = {
    semanticDb.symbols
      .find(
        sym => sym.symbol == symbol
      )
  }

  def parentsFromSignature(
      symbol: String,
      signature: Signature,
      filePath: Option[AbsolutePath]
  ): Seq[(String, ClassLocation)] = {

    def fromClassSignature(
        classSig: ClassSignature
    ): Seq[(String, ClassLocation)] = {
      val allLocations = classSig.parents.collect {
        case t: TypeRef =>
          val loc =
            ClassLocation(
              symbol,
              filePath.map(_.toNIO),
              t,
              classSig.typeParameters
            )
          t.symbol -> loc

      }
      allLocations
    }

    def fromTypeSignature(typeSig: TypeSignature) = {
      typeSig.upperBound match {
        case tr: TypeRef =>
          Seq(
            tr.symbol -> ClassLocation(
              symbol,
              None,
              tr,
              typeSig.typeParameters
            )
          )
        case _ => Seq.empty
      }
    }

    signature match {
      case classSig: ClassSignature =>
        fromClassSignature(classSig)
      case ts: TypeSignature =>
        fromTypeSignature(ts)
      case other =>
        Seq.empty
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
