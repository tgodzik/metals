package scala.meta.internal.metals

import org.eclipse.lsp4j.{DocumentOnTypeFormattingParams, Range, TextEdit}

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.inputs.Input
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Constant
import scala.meta.tokens.Tokens
import scala.meta.tokens.Token.Interpolation
import org.eclipse.lsp4j.DocumentRangeFormattingParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Position

/*in order to use onTypeFormatting in vscode,
you'll have to set editor.formatOnType = true
and editor.formatOnPaste = true in settings*/
final class MultilineStringFormattingProvider(
    semanticdbs: Semanticdbs,
    buffer: Buffers
)(implicit ec: ExecutionContext) {

  private val quote = '"'
  private val space = " "
  private val stripMargin = "stripMargin"

  private def hasStripMarginSuffix(
      stringTokenIndex: Int,
      tokens: Tokens
  ): Boolean = {
    var methodIndex = stringTokenIndex + 1
    while (tokens(methodIndex).isWhiteSpaceOrComment ||
      tokens(methodIndex).isInstanceOf[Token.Dot]) methodIndex += 1
    tokens(methodIndex) match {
      case token: Token.Ident if token.value == stripMargin =>
        true
      case other =>
        false
    }
  }

  private def indent(toInput: String, start: Int): String = {
    val beforePos = toInput.substring(0, start)
    val lastPipe = beforePos.lastIndexOf("|")
    val lastNewline = beforePos.lastIndexOf("\n", lastPipe)
    val indent = beforePos.substring(beforePos.lastIndexOf("\n")).length
    val length = toInput.substring(lastNewline, lastPipe).length
    space * (length - indent)
  }

  private def isMultilineString(text: String, token: Token) = {
    val start = token.start
    text(start) == quote &&
    text(start + 1) == quote &&
    text(start + 2) == quote
  }

  private def inToken(pos: meta.Position, token: Token): Boolean = {
    pos.start >= token.start && pos.end <= token.end
  }

  private def pipeInScope(pos: meta.Position, text: String): Boolean = {
    val firstNewline = text.substring(0, pos.start).lastIndexOf("\n")
    val lastNewline =
      text.substring(0, firstNewline).lastIndexOf("\n")
    text
      .substring(lastNewline + 1, pos.start)
      .contains("|")
  }

  private def multilineStringInTokens(
      tokens: Tokens,
      pos: meta.Position,
      sourceText: String
  ): Boolean = {
    var tokenIndex = 0
    var stringFound = false
    var shouldAddPipes = false
    while (!stringFound && tokenIndex < tokens.size) {
      tokens(tokenIndex) match {
        case token: Constant.String if inToken(pos, token) =>
          stringFound = true
          shouldAddPipes = isMultilineString(sourceText, token) &&
            hasStripMarginSuffix(tokenIndex, tokens)
        case start: Interpolation.Start if start.start < pos.start =>
          var endIndex = tokenIndex + 1
          while (!tokens(endIndex)
              .isInstanceOf[Interpolation.End]) endIndex += 1
          val end = tokens(endIndex)
          stringFound = end.end > pos.end
          shouldAddPipes = stringFound && isMultilineString(sourceText, start) &&
            hasStripMarginSuffix(endIndex, tokens)
        case _ =>
      }
      tokenIndex += 1
    }
    shouldAddPipes
  }

  private def withToken(
      textId: TextDocumentIdentifier,
      range: Range
  )(
      fn: String => meta.Position => List[TextEdit]
  ): Future[List[TextEdit]] = Future {
    val source = textId.getUri.toAbsolutePath
    if (source.exists) {
      val sourceText = buffer.get(source).getOrElse("")
      val pos = range.getStart.toMeta(
        Input.VirtualFile(source.toString(), sourceText)
      )
      if (pipeInScope(pos, sourceText)) {
        val tokens =
          Input.VirtualFile(source.toString(), sourceText).tokenize.toOption
        tokens.toList.flatMap { tokens: Tokens =>
          if (multilineStringInTokens(tokens, pos, sourceText))
            fn(sourceText)(pos)
          else Nil
        }
      } else Nil
    } else Nil
  }

  def format(
      params: DocumentOnTypeFormattingParams
  ): Future[List[TextEdit]] = {
    val range = new Range(params.getPosition, params.getPosition)
    withToken(
      params.getTextDocument(),
      range
    ) { sourceText => position =>
      List(new TextEdit(range, indent(sourceText, position.start) + "|"))
    }
  }

  def format(
      params: DocumentRangeFormattingParams
  ): Future[List[TextEdit]] = {
    val source = params.getTextDocument.getUri.toAbsolutePath
    val range = params.getRange()

    withToken(
      params.getTextDocument(),
      range
    ) { sourceText => position =>
      val lines = (range.getStart().getLine() + 1) to range.getEnd().getLine()
      var start = position.start - 2
      while (start > 0 && sourceText(start) == ' ') {
        start -= 1
      }
      val newText = " " * (position.start - 2 - start) + "|"
      lines.map { line =>
        val pos = new Position(line, 0)
        new TextEdit(new Range(pos, pos), newText)
      }.toList
    }
  }
}
