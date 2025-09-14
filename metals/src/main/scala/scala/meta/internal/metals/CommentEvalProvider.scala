package scala.meta.internal.metals

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta._
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.parsing.Trees
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.io.AbsolutePath
import scala.meta.pc.CancelToken
import scala.meta.tokens.Token.Comment

import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{Position => LSPPosition}
import scala.meta.internal.parsing.TokenOps.syntax.TokenSyntax

/**
 * Provider for evaluating comments that start with ">>>" using worksheet evaluation.
 *
 * This provider:
 * 1. Tokenizes Scala source files
 * 2. Finds all comments that start with ">>>"
 * 3. Groups consecutive comment statements
 * 4. Creates virtual worksheets for each group
 * 5. Evaluates them using the existing WorksheetProvider
 * 6. Returns inlay hints showing the evaluation results
 */
class CommentEvalProvider(
    trees: Trees,
    worksheetProvider: WorksheetProvider,
    buffers: Buffers,
)(implicit ec: ExecutionContext) {

  def inlayHints(
      path: Option[AbsolutePath],
      token: CancelToken,
  ): Future[List[InlayHint]] = {
    path match {
      case Some(path) => inlayHints(path, token)
      case None => Future.successful(Nil)
    }
  }

  def inlayHints(
      path: AbsolutePath,
      token: CancelToken,
  ): Future[List[InlayHint]] = {
    if (path.isScala) {
      extractCommentStatements(path) match {
        case Some(statementGroups) if statementGroups.nonEmpty =>
          evaluateStatementGroups(path, statementGroups, token)
        case _ =>
          Future.successful(Nil)
      }
    } else {
      Future.successful(Nil)
    }
  }

  private def extractCommentStatements(
      path: AbsolutePath
  ): Option[List[CommentStatementGroup]] = {
    try {
      val objects = trees.get(path) match {
        case Some(Source(topLevel)) =>
          topLevel.collect { case obj: Defn.Object =>
            obj
          }
        case _ => Nil
      }
      trees.tokenized(path) match {
        case Some(tokens) =>
          val all = ListBuffer[Comment]()

          tokens.foreach { token =>
            token match {
              case comment: Comment =>
                val commentText = comment.value

                if (commentText.trim().startsWith(">>>")) {
                  val statement = commentText.trim().stripPrefix(">>>").trim()
                  if (statement.nonEmpty) {
                    all += comment
                  }
                }
              case _ =>
            }
          }

          val comments = all.toList
          val groupedByObject = comments
            .groupBy(cm => objects.find(_.pos.encloses(cm.pos)))
            .collect { case (grouped, comments) =>
              CommentStatementGroup(
                comments.map(cm =>
                  CommentStatement(
                    cm.value,
                    cm.pos.startLine,
                    cm.pos.endLine,
                    cm.pos.endColumn,
                  )
                ),
                grouped.map(_.name.syntax),
              )
            }
          // TODO now add at the end and import the object
          if (groupedByObject.nonEmpty) Some(groupedByObject.toList) else None
        case None => None
      }
    } catch {
      case _: Throwable => None
    }
  }

  private def evaluateStatementGroups(
      path: AbsolutePath,
      statementGroups: List[CommentStatementGroup],
      token: CancelToken,
  ): Future[List[InlayHint]] = {
    val evaluationFutures = statementGroups.map { group =>
      evaluateStatementGroup(path, group, token)
    }

    Future.sequence(evaluationFutures).map(_.flatten)
  }

  private def evaluateStatementGroup(
      path: AbsolutePath,
      group: CommentStatementGroup,
      token: CancelToken,
  ): Future[List[InlayHint]] = {
    // Create a single string with all statements in the group
    val prev = buffers.get(path).getOrElse("") + "\n"
    val withImport =
      group.fromObject.map(obj => s"import $obj._\n").getOrElse("")
    val worksheetContent = group.statements
      .map(_.statement.trim().stripPrefix(">>>").trim())
      .mkString("\n")
    val prefix = prev + withImport
    val originalLines = prev.count(_ == '\n')

    val fullContent = prefix + worksheetContent

    // Use the new evaluateString method from WorksheetProvider
    worksheetProvider
      .evaluateString(fullContent, path, token)
      .map {
        case Some(evaluatedWorksheet) =>
          createInlayHintsWithResults(group, evaluatedWorksheet, originalLines)
        case None =>
          createPlaceholderHints(group)
      }
      .recover { case _: Throwable =>
        createPlaceholderHints(group)
      }
  }

  private def createInlayHintsWithResults(
      group: CommentStatementGroup,
      evaluatedWorksheet: mdoc.interfaces.EvaluatedWorksheet,
      originalLines: Int,
  ): List[InlayHint] = {
    val statements = evaluatedWorksheet
      .statements()
      .asScala
      .toList
      .dropWhile(_.position().startLine() < originalLines)
    // Map worksheet results back to comment positions
    group.statements.zipWithIndex.flatMap { case (commentStatement, index) =>
      statements.lift(index).map { workSheetStatement =>
        val position =
          new LSPPosition(commentStatement.endLine, commentStatement.endColumn)
        val result = truncatify(workSheetStatement)
        val hint = new InlayHint(
          position,
          messages.Either.forLeft(s" // $result"),
        )
        hint.setTooltip(workSheetStatement.details())
        hint
      }
    }
  }

  private def createPlaceholderHints(
      group: CommentStatementGroup
  ): List[InlayHint] = {
    group.statements.map { statement =>
      val position = new LSPPosition(statement.endLine, statement.endColumn)
      val hint = new InlayHint(
        position,
        messages.Either.forLeft(" // <evaluation failed>"),
      )
      hint.setTooltip("Comment evaluation failed")
      hint
    }
  }

  private def truncatify(
      statement: mdoc.interfaces.EvaluatedWorksheetStatement
  ): String = {
    val output = statement.summary()
    if (output.length > 100) output.take(97) + "..."
    else output
  }

}

case class CommentStatement(
    statement: String,
    startLine: Int,
    endLine: Int,
    endColumn: Int,
)

case class CommentStatementGroup(
    statements: List[CommentStatement],
    fromObject: Option[String],
)
