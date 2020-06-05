package scala.meta.internal.metals

import scala.meta.Defn
import scala.meta.Pkg
import scala.meta.Tree
import scala.meta.pc.OffsetParams
import scala.meta.Template
import scala.meta.Term
import scala.collection.concurrent.TrieMap

object ClassFinder {

  def findNSymbolInTemplate(block: Template)(tree: Tree) = {
    val num = TrieMap.empty[String, Int]
    def loop(currentTree: Tree): Option[Int] = {
      if (currentTree == tree) {
        // TODO use map and check by names
        Some(num)
      } else {
        currentTree match {
          case _: Pkg.Object | _: Defn.Trait | _: Defn.Class =>
            num += 1
          case _: Defn.Object =>
            num += 2
          case _ =>
        }
        currentTree.children.map(loop).headOption.flatten
      }
    }
    loop(block)
  }

  def findClassForOffset(
      tree: Tree,
      pos: OffsetParams,
      symbol: String = "",
      isInnerClass: Boolean = false,
      isInBlock: Boolean = false,
      isNAnonymous: Tree => Option[Int] = _ => None
  ): String = {
    val delimeter =
      if (symbol.endsWith("$")) ""
      else if (isInnerClass) "$"
      else if (symbol.isEmpty()) ""
      else "."

    def findChild(
        fullName: String,
        isInner: Boolean,
        inBlock: Boolean = isInBlock,
        findAnon: Tree => Option[Int] = isNAnonymous
    ) = {
      tree.children.find { child =>
        child.pos.start < pos.offset && pos.offset < child.pos.end
      } match {
        case None => fullName
        case Some(value) =>
          findClassForOffset(
            value,
            pos,
            fullName,
            isInner,
            inBlock,
            findAnon
          )
      }
    }

    val anonymousName = if (isInBlock) {
      isNAnonymous(tree).map("$" + _).getOrElse("")
    } else {
      ""
    }
    tree match {
      case Pkg(ref, _) =>
        val name = ref.toString()
        findChild(symbol + delimeter + name, isInner = false)

      case obj: Pkg.Object =>
        val prefix = if (symbol.isEmpty()) "" else "."
        val name = obj.name.toString()
        findChild(
          symbol + prefix + name + ".package" + "$",
          isInner = true
        )

      case obj: Defn.Object =>
        val name = obj.name.toString() + anonymousName
        findChild(
          symbol + delimeter + name + "$",
          isInner = true
        )

      case cls: Defn.Class =>
        val name = cls.name.toString() + anonymousName
        findChild(symbol + delimeter + name, isInner = true)

      case trt: Defn.Trait =>
        val name = trt.name.toString() + anonymousName
        findChild(symbol + delimeter + name, isInner = true)

      case tempalte: Template =>
        findChild(
          symbol,
          isInner = true,
          findAnon = findNSymbolInTemplate(tempalte)
        )
      case block: Term.Block =>
        findChild(
          symbol,
          isInner = true,
          inBlock = true,
          isNAnonymous
        )

      case _ =>
        findChild(symbol, isInnerClass)
    }

  }
}
