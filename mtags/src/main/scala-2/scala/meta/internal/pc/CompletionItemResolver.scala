package scala.meta.internal.pc

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.mtags.MtagsEnrichments._

import org.eclipse.lsp4j.CompletionItem

class CompletionItemResolver(
    val compiler: MetalsGlobal
) extends ItemResolver {
  import compiler._
  def resolve(item: CompletionItem, msym: String): CompletionItem = {

    val data = item.data.getOrElse(CompletionItemData.empty)

    if (
      data.kind == CompletionItemData.ImplementAllKind &&
      !data.additionalSymbols.isEmpty()
    ) {
      data.additionalSymbols.asScala.foldLeft(item) { case (item, sym) =>
        handleSymbol(item, sym)
      }
    } else {
      handleSymbol(item, msym)
    }
  }

  private def handleSymbol(item: CompletionItem, msym: String) = {
    val gsym = inverseSemanticdbSymbol(msym)
    if (gsym != NoSymbol) {
      symbolDocumentation(gsym, _ => Nil).orElse(
        symbolDocumentation(gsym.companion, _ => Nil)
      ) match {
        case Some(info) if item.getDetail != null =>
          enrichDocs(
            item,
            info,
            metalsConfig,
            fullDocstring(gsym, _ => Nil),
            isJavaSymbol(gsym)
          )
        case _ =>
          item
      }
    } else {
      item
    }
  }

  def fullDocstring(gsym: Symbol, lookup: Name => List[NameLookup]): String = {
    def docs(gsym: Symbol): String =
      symbolDocumentation(gsym, lookup).fold("")(_.docstring())
    val gsymDoc = docs(gsym)
    def keyword(gsym: Symbol): String =
      if (gsym.isClass) "class"
      else if (gsym.isTrait) "trait"
      else if (gsym.isJavaInterface) "interface"
      else if (gsym.isModule) "object"
      else ""
    val companion = gsym.companion
    if (companion == NoSymbol || isJavaSymbol(gsym)) {
      if (gsymDoc.isEmpty) {
        if (gsym.isAliasType) {
          fullDocstring(gsym.info.dealias.typeSymbol, lookup)
        } else if (gsym.isMethod) {
          gsym.info.finalResultType match {
            case SingleType(_, sym) =>
              fullDocstring(sym, lookup)
            case _ =>
              ""
          }
        } else ""
      } else {
        gsymDoc
      }
    } else {
      val companionDoc = docs(companion)
      if (companionDoc.isEmpty) gsymDoc
      else if (gsymDoc.isEmpty) companionDoc
      else {
        List(
          s"""|### ${keyword(companion)} ${companion.name}
              |$companionDoc
              |""".stripMargin,
          s"""|### ${keyword(gsym)} ${gsym.name}
              |${gsymDoc}
              |""".stripMargin
        ).sorted.mkString("\n")
      }
    }
  }
}
