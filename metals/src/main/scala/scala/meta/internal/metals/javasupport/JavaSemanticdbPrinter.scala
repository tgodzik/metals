package scala.meta.internal.metals.javasupport

import scala.meta.internal.mtags.Semanticdbs
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.TextDocumentPositionParams
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.pc.CancelToken
import scala.meta.internal.semanticdb.SymbolInformation
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.internal.semanticdb.Signature.Empty
import scala.meta.internal.semanticdb.ClassSignature
import scala.meta.internal.semanticdb.MethodSignature
import scala.meta.internal.semanticdb.TypeSignature
import scala.meta.internal.semanticdb.ValueSignature
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.metals.HoverExtParams
import scala.meta.internal.semanticdb.TypeRef

// TODO add globacl cache to print info on all symbols
class JavaSemanticdbPrinter(
    semanticdbs: Semanticdbs,
    definitionProvider: DefinitionProvider
) {

  def hover(
      params: HoverExtParams,
      token: CancelToken
  )(implicit ctx: ExecutionContext): Future[Hover] = {
    val source = params.textDocument.getUri.toAbsolutePath

    val textDocumentPositionParams =
      new TextDocumentPositionParams(params.textDocument, params.position)
    definitionProvider
      .definition(source, textDocumentPositionParams, token)
      .map { definition =>
        val hover = for {
          doc <- definition.semanticdb
          symbol = definition.symbol
          info <- doc.symbols.find(_.symbol == symbol)
          sig <- printSymbolInformation(info, doc)
        } yield new Hover(
          s"""|```java
              |$sig
              |```""".stripMargin.toMarkupContent
        )
        hover.getOrElse(null)

      }
  }

  private def printSymbolInformation(
      info: SymbolInformation,
      textDoc: s.TextDocument
  ): Option[String] = {

    val name = info.displayName
    val printer = new Printer(textDoc)
    info.signature match {
      case Empty => None
      // TODO type params?
      case ClassSignature(_, parents, _, _) =>
        pprint.log(parents.headOption)
        val parentsString =
          parents.headOption
            .filter {
              case TypeRef(_, symbol, _) if symbol == "java/lang/Object#" =>
                false
              case _ => true
            }
            .map(tpe => s" extends ${printer.printType(tpe)}")
            .getOrElse("")

        Some(s"class ${name}${parentsString}")
      // TODO type params, params?
      case MethodSignature(_, parameterLists, returnType) =>
        val retTypeString = printer.printType(returnType)
        val params = printer.printScope(parameterLists.headOption)
        Some(s"$retTypeString $name($params)")
      case _: TypeSignature => None
      case ValueSignature(tpe) =>
        val tpeString = printer.printType(tpe)
        Some(s"$tpeString $name")
    }
  }

  class Printer(textDoc: s.TextDocument) {

    private def printSymbol(symbol: String): String = {
      pprint.log(textDoc.symbols.find(_.symbol == symbol).map(_.displayName))
      pprint.log(textDoc)
      textDoc.symbols
        .find(_.symbol == symbol)
        .map(_.displayName)
        .getOrElse(symbol)
    }

    def printType(t: s.Type): String =
      t match {
        case s.Type.Empty => ""
        case s.RepeatedType(tpe) =>
          s"${printType(tpe)}*"
        case s.SingleType(prefix, symbol) =>
          s"${printPrefix(prefix)}${printSymbol(symbol)}"
        case s.TypeRef(_, "scala/Unit#", _) => "void"
        case s.TypeRef(prefix, symbol, typeArguments) =>
          // don't print unnamed types
          val sym = printSymbol(symbol)
          val typeArgs = printTypeArgs(typeArguments)
          s"${printPrefix(prefix)}${sym}${typeArgs}"
        case s.WithType(types) =>
          val simpleTypes = types.dropWhile {
            case s.TypeRef(_, sym, _) =>
              sym == "scala/AnyRef#" || sym == "java/lang/Object#"
            case _ => false
          }
          simpleTypes.map(printType).mkString(" with ")
        case s.ConstantType(constant) =>
          printConstant(constant)
        case s.ThisType(symbol) =>
          s"this.${printSymbol(symbol)}"
        case s.IntersectionType(types) =>
          types.map(printType).mkString(" & ")
        case s.UnionType(types) =>
          types.map(printType).mkString(" | ")
        case s.SuperType(prefix, symbol) =>
          s"super.${printPrefix(prefix)}${printSymbol(symbol)}"
        case s.AnnotatedType(annots, tp) =>
          val mapped = annots
            .map(x => s"@${printType(x.tpe)}")
            .reduceLeft((x, y) => s"$x $y")
          s"$mapped ${printType(tp)}"
        case s.StructuralType(tpe, scope) =>
          s"${printType(tpe)} {${printScope(scope)}}"
        case _ => throw new Exception("")

      }

    def printScope(scope: Option[s.Scope]): String = {
      scope match {
        case Some(value) if value.symlinks.nonEmpty =>
          value.symlinks
            .map { symLink =>
              textDoc.symbols
                .collectFirst {
                  case info if info.symbol == symLink =>
                    printSymbolInformation(info, textDoc)
                }
                .flatten
                .getOrElse(symLink)
            }
            .mkString(", ")

        case _ => ""
      }
    }

    def printPrefix(t: s.Type): String = {
      printType(t) match {
        case "" => ""
        case s => s"$s."
      }
    }

    def printTypeArgs(
        typeArgs: Seq[s.Type]
    ): String =
      typeArgs match {
        case Nil => ""
        case _ =>
          typeArgs.map(printType).mkString("<", ", ", ">")
      }

    def printConstant(c: s.Constant): String =
      c match {
        case s.FloatConstant(value) => value.toString
        case s.LongConstant(value) => value.toString
        case s.DoubleConstant(value) => value.toString
        case s.NullConstant() => "null"
        case s.IntConstant(value) => value.toString
        case s.CharConstant(value) => value.toString
        case s.ByteConstant(value) => value.toString
        case s.UnitConstant() => "()"
        case s.ShortConstant(value) => value.toString
        case s.Constant.Empty => ""
        case s.BooleanConstant(value) => value.toString
        case s.StringConstant(value) => value
      }
  }

}
