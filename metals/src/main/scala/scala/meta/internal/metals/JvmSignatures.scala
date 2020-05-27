package scala.meta.internal.metals
import scala.annotation.tailrec

import scala.meta.internal.semanticdb.Scala.Symbols
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence

object JvmSignatures {
  private val shouldIgnore = Set(Symbols.RootPackage, Symbols.EmptyPackage)

  def toTypeSignature(definition: SymbolOccurrence): TypeSignature = {
    @tailrec
    def loop(owners: List[String], fqcn: StringBuilder): String = {
      owners match {
        case Nil => fqcn.toString()
        case symbol :: tail if shouldIgnore(symbol) =>
          loop(tail, fqcn)
        case symbol :: tail =>
          val desc = symbol.desc
          fqcn.append(desc.name)

          val delimiter =
            if (desc.isPackage) "."
            else if (desc.isTerm) "$"
            else if (tail.nonEmpty) "$" // nested class
            else ""
          loop(tail, fqcn.append(delimiter))
      }
    }

    val cls = findClass(definition.symbol)
    val fqcn = loop(cls.ownerChain, new StringBuilder)
    TypeSignature(fqcn)
  }

  private def findClass(symbol: String): String = {
    val desc = symbol.desc
    if (desc.isTerm || desc.isType) {
      symbol
    } else {
      findClass(symbol.owner)
    }
  }

  final case class TypeSignature(value: String) {
    override def toString: String = value
  }
}
