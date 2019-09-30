package scala.meta.internal.implementation
import scala.meta.internal.semanticdb.SymbolInformation
import java.nio.file.Path
import scala.collection.mutable

case class LocalClassDefinitions(
    typeAliases: Map[String, String],
    inheritance: Map[String, Set[ClassLocation]]
)

case class InheritanceContext(
    findSymbol: String => Option[SymbolInformation],
    private val aliases: Map[String, String],
    private val inheritance: Map[String, Set[ClassLocation]]
) {
  protected lazy val reverseAliases: Map[String, String] =
    aliases.map(a => a._2 -> a._1).toMap

  def aliasFor(symbol: String): String = {
    dealias(reverseAliases, symbol)
  }

  def forAlias(symbol: String): String = {
    dealias(aliases, symbol)
  }

  def withClasspathContext(
      classpathAliases: Map[String, String],
      classpathInheritance: Map[String, Set[ClassLocation]],
      findGlobalSymbol: String => Option[SymbolInformation]
  ): InheritanceContext = {
    val newInheritance = mutable.Map
      .empty[String, Set[ClassLocation]] ++ inheritance
    for { (symbol, locations) <- classpathInheritance } {
      val newLocations = newInheritance.getOrElse(symbol, Set.empty) ++ locations
      newInheritance += symbol -> newLocations
    }
    this.copy(
      findSymbol = findGlobalSymbol,
      aliases = classpathAliases ++ aliases,
      inheritance = newInheritance.toMap
    )
  }

  private def dealias(
      typeMap: Map[String, String],
      symbol: String
  ): String = {
    if (typeMap.contains(symbol)) {
      var sym = symbol
      while (typeMap.contains(sym)) sym = typeMap(sym)
      sym
    } else symbol
  }

}

object InheritanceContext {

  def apply(
      findSymbol: String => Option[SymbolInformation],
      localDefinitions: Map[Path, LocalClassDefinitions]
  ): InheritanceContext = {
    val typeAliases = mutable.Map.empty[String, String]
    val inheritance = mutable.Map
      .empty[String, Set[ClassLocation]]
    for {
      (_, definitions) <- localDefinitions
      _ = typeAliases ++= definitions.typeAliases
      (symbol, locations) <- definitions.inheritance
    } {
      val updated = inheritance.getOrElse(symbol, Set.empty) ++ locations
      inheritance += symbol -> updated
    }
    InheritanceContext(findSymbol, typeAliases.toMap, inheritance.toMap)
  }
}
