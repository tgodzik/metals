package scala.meta.internal.pc

import java.nio.file.Paths

import scala.annotation.tailrec
import scala.meta as m

import scala.meta.internal.metals.ReportContext
import scala.meta.internal.mtags.MtagsEnrichments.*
import scala.meta.internal.pc.printer.MetalsPrinter
import scala.meta.internal.pc.printer.ShortenedNames
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.TextEdit
import m.internal.mtags.KeywordWrapper

final class InferredMethodProvider(
    params: OffsetParams,
    driver: InteractiveDriver,
    config: PresentationCompilerConfig,
    symbolSearch: SymbolSearch,
)(using ReportContext):

  case class AdjustTypeOpts(
      text: String,
      adjustedEndPos: l.Position,
  )

  val uri = params.uri
  val filePath = Paths.get(uri)
  val sourceText = params.text
  val source =
    SourceFile.virtual(filePath.toString, sourceText)
  driver.run(uri, source)
  val unit = driver.latestRun
  val pos = driver.sourcePosition(params)
  val path =
    Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

  given locatedCtx: Context = driver.localContext(params)
  val indexedCtx = IndexedContext(locatedCtx)
  val autoImportsGen = AutoImports.generator(
    pos,
    params.text,
    unit.tpdTree,
    indexedCtx,
    config,
  )
  val shortenedNames = new ShortenedNames(indexedCtx)

  def imports: List[TextEdit] =
    shortenedNames.imports(autoImportsGen)

  def insertPosition() =
    val blockOrTemplateIndex =
      path.tail.indexWhere {
        case _: Block[?] | _: Template[?] => true
        case _ => false
      }
    path(blockOrTemplateIndex).sourcePos

  def indentation(text: String, pos: Int): String =
    if pos > 0 then
      val isSpace = text(pos) == ' '
      val isTab = text(pos) == '\t'
      val indent =
        countIndent(params.text(), pos, 0)

      if isSpace then " " * indent else if isTab then "\t" * indent else ""
    else ""

  @tailrec
  def countIndent(text: String, index: Int, acc: Int): Int =
    if text(index) != '\n' then countIndent(text, index - 1, acc + 1)
    else acc

  def printType(tpe: Type): String =
    val printer = MetalsPrinter.forInferredType(
      shortenedNames,
      indexedCtx,
      symbolSearch,
      includeDefaultParam = MetalsPrinter.IncludeDefaultParam.ResolveLater,
    )
    printer.tpe(tpe)

  def paramsString(params: List[Type]) =
    params.zipWithIndex
      .map { case (p, index) =>
        s"arg$index: ${printType(p)}"
      }
      .mkString(", ")

  def inferredMethodEdits(): List[TextEdit] =
    path match
      case (id @ Ident(errorMethod)) :: (apply @ Apply(
            Ident(containingMethod),
            arguments,
          )) :: Apply(sel: Select[?], outerArgs) :: _
          if id.symbol == NoSymbol =>

        val ret = sel.symbol.info
        ret match
          case m @ MethodType(param) =>
            val currentArgIndex = outerArgs.indexOf(apply)
            val retType = printType(m.paramInfos(currentArgIndex))

            val argumentsTypes = arguments.map(_.typeOpt.widenDealias)
            val methodParams = paramsString(argumentsTypes)
            val methodName =
              KeywordWrapper.Scala3.backtickWrap(errorMethod.toString())
            val signature =
              s"def ${methodName}($methodParams): $retType = ???"
            val pos = insertPosition()
            val indent = indentation(params.text(), pos.start - 1)
            val lspPos = pos.toLsp
            lspPos.setEnd(lspPos.getStart())
            List(
              TextEdit(
                lspPos,
                s"$signature\n$indent",
              )
            )
          case _ =>
            Nil
        end match

      case other =>
        Nil

end InferredMethodProvider
