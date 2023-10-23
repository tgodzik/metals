package scala.meta.internal.pc

import java.nio.file.Path

import scala.util.control.NonFatal

import scala.meta.pc.SymbolSearchVisitor

import org.eclipse.{lsp4j => l}

trait WorkspaceSymbolSearch { compiler: MetalsGlobal =>

  def searchOutline(
      visitMember: Symbol => Boolean,
      query: String
  ) {

    def traverseUnit(unit: RichCompilationUnit) = {

      class Traverser extends compiler.Traverser {
        override def traverse(tree: Tree): Unit = {
          val sym = tree.symbol
          def matches = if (sym.isType)
            CompletionFuzzy.matchesSubCharacters(query, sym.name.toString())
          else CompletionFuzzy.matches(query, sym.name.toString())
          if (sym.exists && matches) { // && !sym.isStale
            try {
              visitMember(sym)
            } catch {
              case _: Throwable =>
              // with outline compiler there might be situations when things fail
            }
          }
        }
      }
      unit.body.traverse(new Traverser)
    }
    compiler.richCompilationCache.values.foreach(traverseUnit)
  }

  class CompilerSearchVisitor(
      context: Context,
      visitMember: Symbol => Boolean
  ) extends SymbolSearchVisitor {

    def visit(top: SymbolSearchCandidate): Int = {

      var added = 0
      for {
        sym <- loadSymbolFromClassfile(top)
        if context.lookupSymbol(sym.name, _ => true).symbol != sym
      } {
        if (visitMember(sym)) {
          added += 1
        }
      }
      added
    }
    def visitClassfile(pkg: String, filename: String): Int = {
      visit(SymbolSearchCandidate.Classfile(pkg, filename))
    }
    def visitWorkspaceSymbol(
        path: Path,
        symbol: String,
        kind: l.SymbolKind,
        range: l.Range
    ): Int = {
      visit(SymbolSearchCandidate.Workspace(symbol, path))
    }

    def shouldVisitPackage(pkg: String): Boolean =
      packageSymbolFromString(pkg).isDefined

    override def isCancelled: Boolean = {
      false
    }
  }

  private def loadSymbolFromClassfile(
      classfile: SymbolSearchCandidate
  ): List[Symbol] = {
    def isAccessible(sym: Symbol): Boolean = {
      sym != NoSymbol && {
        sym.info // needed to fill complete symbol
        sym.isPublic
      }
    }
    try {
      classfile match {
        case SymbolSearchCandidate.Classfile(pkgString, filename) =>
          val pkg = packageSymbolFromString(pkgString).getOrElse(
            throw new NoSuchElementException(pkgString)
          )
          val names = filename
            .stripSuffix(".class")
            .split('$')
            .iterator
            .filterNot(_.isEmpty)
            .toList
          val members = names.foldLeft(List[Symbol](pkg)) {
            case (accum, name) =>
              accum.flatMap { sym =>
                if (!isAccessible(sym) || !sym.isModuleOrModuleClass) Nil
                else {
                  sym.info.member(TermName(name)) ::
                    sym.info.member(TypeName(name)) ::
                    Nil
                }
              }
          }
          members.filter(sym => isAccessible(sym))
        case SymbolSearchCandidate.Workspace(symbol, path)
            if !compiler.isOutlinedFile(path) =>
          // pprint.log(path)
          val gsym = inverseSemanticdbSymbol(symbol)
          if (isAccessible(gsym)) gsym :: Nil
          else Nil
        case _ =>
          Nil
      }
    } catch {
      case NonFatal(_) => Nil
    }
  }
}
