package scala.meta.internal.pc

import org.eclipse.lsp4j.DocumentHighlight
import scala.meta.pc.OffsetParams
import scala.collection.mutable.ListBuffer

class DocumentHighlightProvider(
    val compiler: MetalsGlobal,
    params: OffsetParams
) {
  private val definitionProvider = new PcDefinitionProvider(compiler, params)

  import definitionProvider.compiler._
  def documentHighlight(): List[DocumentHighlight] = {
    val unit = addCompilationUnit(
      params.text(),
      params.uri().toString(),
      None
    )
    typeCheck(unit)
    val pos = unit.position(params.offset())
    val tree = definitionProvider.definitionTypedTreeAt(pos)

    val foundSym = tree match {
      // find named argument, TODO check if works in definition provider
      case TreeApply(fun, elements)
          if !fun.pos.includes(pos) && !elements.exists(_.pos.includes(pos)) =>
        fun.tpe match {
          case MethodType(params, _) =>
            val index = elements.indexWhere(p => pos.isAfter(p.pos))
            params(index + 1)
          case _ =>
            tree.symbol
        }
      case _ =>
        tree.symbol
    }

    val seek = Set(
      foundSym.localName,
      foundSym.getterName,
      foundSym.setterName,
      foundSym.companion.name
    )
    // TODO find also named params
    val trueOwner =
      if (foundSym.owner.isCaseApplyOrUnapply)
        foundSym.owner.owner.companionClass
      else foundSym.owner

    val soughtSymbols = trueOwner.info.decls.collect {
      case d if seek(d.name) =>
        // TODO also check if it's exactly the same symbol
        pprint.log(d)
        d.thisSym
    }

    // pprint.log(soughtSymbols)
    class TreeRaverser() extends Traverser {
      val buffer = ListBuffer[Position]()
      private def isSameSymbol(tr: Tree): Boolean = {
        soughtSymbols.exists(_ == tr.symbol) && tr.pos.isRange
      }
      override def traverse(tree: Tree): Unit = {
        tree match {
          case tr @ Select(_, _) if isSameSymbol(tr) =>
            buffer += tr.namePos
          case tr @ ValDef(_, _, _, _) if isSameSymbol(tr) =>
            buffer += tr.namePos
          case tr: DefDef if isSameSymbol(tr) =>
            buffer += tr.namePos
          case tr @ Apply(_, _) if isSameSymbol(tr) =>
          case tr if isSameSymbol(tr) =>
            buffer += (tr.pos)
          case _ =>
          // pprint.log(tree)
        }
        super.traverse(tree)
      }
    }

    if (!foundSym.isErroneous) {
      val traverser = new TreeRaverser()
      traverser.traverse(unit.lastBody)
      traverser.buffer.result().map(pos => new DocumentHighlight(pos.toLSP))
    } else
      Nil
  }
}
