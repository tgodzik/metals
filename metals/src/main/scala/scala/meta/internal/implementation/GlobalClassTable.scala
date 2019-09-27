package scala.meta.internal.implementation
import scala.meta.internal.metals.BuildTargets
import scala.meta.io.AbsolutePath
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io.Classpath
import scala.collection.concurrent.TrieMap
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import scala.meta.internal.semanticdb.SymbolInformation

final class GlobalClassTable(
    buildTargets: BuildTargets
) {

  case class ClassIndex(
      symtab: GlobalSymbolTable,
      inheritance: Map[String, Set[ClassLocation]]
  )
  private val buildTargetsIndexes =
    TrieMap.empty[BuildTargetIdentifier, ClassIndex]

  def indexFor(source: AbsolutePath): Option[ClassIndex] = {
    for {
      buildTargetId <- buildTargets.inverseSources(source)
      index <- buildTargetsIndexes.get(buildTargetId)
    } yield index
  }

  def indexFor(
      source: AbsolutePath,
      classes: Set[String]
  ): Option[ClassIndex] = synchronized {
    for {
      buildTargetId <- buildTargets.inverseSources(source)
      index <- calculateIndex(buildTargetId, classes)
    } yield {
      buildTargetsIndexes
        .getOrElseUpdate(
          buildTargetId,
          index
        )
    }
  }

  private def calculateIndex(
      buildTargetId: BuildTargetIdentifier,
      classes: Set[String]
  ): Option[ClassIndex] = {
    for {
      scalaTarget <- buildTargets.scalaTarget(buildTargetId)
      classpath = new Classpath(scalaTarget.classpath)
      symTab = GlobalSymbolTable(classpath, includeJdk = true)
    } yield {
      val symbolsInformation = for {
        classSymbol <- classes
        classInfo <- symTab.info(classSymbol)
      } yield classInfo

      val inheritance =
        calculateInheritance(symbolsInformation, classes, symTab)

      val locations = inheritance.groupBy(_._1).map {
        case (symbol, locations) =>
          symbol -> locations.map(_._2).toSet
      }
      ClassIndex(symTab, locations)
    }

  }

  private def calculateInheritance(
      allInfos: Set[SymbolInformation],
      calculated: Set[String],
      symTab: GlobalSymbolTable
  ): Set[(String, ClassLocation)] = {
    val allParents = allInfos.flatMap { info =>
      ImplementationProvider.parentsFromSignature(
        info.symbol,
        info.signature,
        None,
        None,
        symTab.info(_)
      )
    }

    val toCalculate = (allParents.map(_._1) -- calculated)
    if (toCalculate.isEmpty) {
      allParents
    } else {
      val newInfos = toCalculate.flatMap(symTab.info)
      allParents ++ calculateInheritance(
        newInfos,
        toCalculate ++ calculated,
        symTab
      )
    }
  }
}
