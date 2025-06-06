package scala.meta.internal.parsing

import java.util.logging.Logger

import scala.annotation.tailrec
import scala.collection.compat.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success

import scala.meta.Input
import scala.meta.Position
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalaVersionSelector
import scala.meta.internal.parsing.TokenOps.syntax._
import scala.meta.internal.{semanticdb => s}
import scala.meta.tokenizers.Tokenized

import ch.epfl.scala.{bsp4j => b}
import difflib._
import org.eclipse.{lsp4j => l}

/**
 * Helper to map between position between two similar strings.
 */
sealed trait TokenEditDistance {

  /**
   * Converts a range position in the original document to a range position in the revised document.
   *
   * This method behaves differently from the other `toRevised` in a few ways:
   * - it should only return `None` in the case when the sources don't tokenize.
   *   When the original token is removed in the revised document, we find instead the
   *   nearest token in the original document instead.  This behaviour can be toggled
   *   off by providing value of false for `fallbackToNearest`.
   *
   * If `adjustWithinToken` is true and the original and revised range are both contained within
   * a single (matching) token, then adjust the returned range to match the start/end offsets of
   * the input range with respect to the token.
   */
  def toRevised(
      range: l.Range,
      adjustWithinToken: Boolean = false,
      fallbackToNearest: Boolean = true,
  ): Option[l.Range]
  def toRevised(originalOffset: Int): Either[EmptyResult, Position]
  def toRevised(
      originalLine: Int,
      originalColumn: Int,
  ): Either[EmptyResult, Position]

  def toOriginal(
      revisedLine: Int,
      revisedColumn: Int,
  ): Either[EmptyResult, Position]

  def toOriginal(revisedOffset: Int): Either[EmptyResult, Position]

  final def toRevised(pos: l.Position): Either[EmptyResult, Position] = {
    toRevised(pos.getLine, pos.getCharacter)
  }
  final def toRevisedStrict(range: s.Range): Option[s.Range] = {
    this match {
      case TokenEditDistance.Unchanged => Some(range)
      case _ =>
        (
          toRevised(range.startLine, range.startCharacter),
          toRevised(range.endLine, range.endCharacter),
        ) match {
          case (Right(start), Right(end)) =>
            Some(
              s.Range(
                start.startLine,
                start.startColumn,
                end.startLine,
                end.startColumn,
              )
            )
          case _ => None
        }
    }
  }

  final def toOriginalStrict(range: s.Range): Option[s.Range] = {
    this match {
      case TokenEditDistance.Unchanged => Some(range)
      case _ =>
        (
          toOriginal(range.startLine, range.startCharacter),
          toOriginal(range.endLine, range.endCharacter),
        ) match {
          case (Right(start), Right(end)) =>
            Some(
              s.Range(
                start.startLine,
                start.startColumn,
                end.endLine,
                end.endColumn,
              )
            )
          case _ => None
        }
    }
  }

  final def toRevised(
      textEdit: l.TextEdit,
      adjustWithinToken: Boolean,
      fallbackToNearest: Boolean,
  ): Option[l.TextEdit] = {
    val revisedRange = toRevised(
      range = textEdit.getRange(),
      adjustWithinToken = adjustWithinToken,
      fallbackToNearest = fallbackToNearest,
    )
    revisedRange.map(r => new l.TextEdit(r, textEdit.getNewText()))
  }

  final def toRevised(
      scalaAction: b.ScalaAction,
      adjustWithinToken: Boolean,
      fallbackToNearest: Boolean,
  ): Option[b.ScalaAction] = {
    val existingEdits = scalaAction.asLspTextEdits
    existingEdits
      .map { textEdit =>
        toRevised(
          textEdit = textEdit,
          adjustWithinToken = adjustWithinToken,
          fallbackToNearest = fallbackToNearest,
        )
      }
      .foldLeft[Option[Seq[l.TextEdit]]](Some(Seq.empty)) {
        case (Some(seq), Some(e)) => Some(seq :+ e)
        case _ => None
      }
      .map { newEdits =>
        val newAction = new b.ScalaAction(scalaAction.getTitle())
        newAction.setEditFromLspTextEdits(newEdits)
        newAction
      }
  }

  /**
   * Converts a bsp diagnostic in the original document to the revised document.
   * `None` will be returned if conversion of any of the diagnostic's underlying
   * text edits cannot be revised.
   */
  final def toRevised(
      scalaDiagnostic: b.ScalaDiagnostic,
      adjustWithinToken: Boolean,
      fallbackToNearest: Boolean,
  ): Option[b.ScalaDiagnostic] = {
    val actions = scalaDiagnostic.getActions()
    val newActions = actions.asScala
      .map { action =>
        toRevised(
          action,
          adjustWithinToken = adjustWithinToken,
          fallbackToNearest = fallbackToNearest,
        )
      }
      .foldLeft[Option[Seq[b.ScalaAction]]](Some(Seq.empty)) {
        case (Some(seq), Some(e)) => Some(seq :+ e)
        case _ => None
      }
    newActions.map { scalaActions =>
      val newDiagnostic = new b.ScalaDiagnostic()
      newDiagnostic.setActions(scalaActions.asJava)
      newDiagnostic
    }
  }
}

object TokenEditDistance {

  case object Unchanged extends TokenEditDistance {
    def toRevised(
        range: l.Range,
        adjustWithinToken: Boolean = false,
        fallbackToNearest: Boolean = true,
    ): Option[l.Range] = Some(range)
    def toRevised(originalOffset: Int): Either[EmptyResult, Position] =
      EmptyResult.unchanged
    def toRevised(
        originalLine: Int,
        originalColumn: Int,
    ): Either[EmptyResult, Position] =
      EmptyResult.unchanged

    def toOriginal(
        revisedLine: Int,
        revisedColumn: Int,
    ): Either[EmptyResult, Position] =
      EmptyResult.unchanged

    def toOriginal(revisedOffset: Int): Either[EmptyResult, Position] =
      EmptyResult.unchanged

    override def toString(): String = "unchanged"
  }

  case object NoMatch extends TokenEditDistance {
    def toRevised(
        range: l.Range,
        adjustWithinToken: Boolean = false,
        fallbackToNearest: Boolean = true,
    ): Option[l.Range] = None
    def toRevised(originalOffset: Int): Either[EmptyResult, Position] =
      EmptyResult.noMatch
    def toRevised(
        originalLine: Int,
        originalColumn: Int,
    ): Either[EmptyResult, Position] =
      EmptyResult.noMatch

    def toOriginal(
        revisedLine: Int,
        revisedColumn: Int,
    ): Either[EmptyResult, Position] =
      EmptyResult.noMatch

    def toOriginal(revisedOffset: Int): Either[EmptyResult, Position] =
      EmptyResult.noMatch

    override def toString(): String = "no-match"
  }

  final class Diff[A](
      matching: Array[MatchingToken[A]],
      originalInput: Input.VirtualFile,
      revisedInput: Input.VirtualFile,
  )(implicit ops: TokenOps[A])
      extends TokenEditDistance {

    private val logger: Logger = Logger.getLogger(this.getClass.getName)
    def toRevised(
        range: l.Range,
        adjustWithinToken: Boolean = false,
        fallbackToNearest: Boolean = true,
    ): Option[l.Range] = {
      range.toMeta(originalInput).flatMap { pos =>
        val matchingTokens = matching.lift

        // Perform two binary searches to find the revised start/end positions.
        // NOTE. I tried abstracting over the two searches since they are so similar
        // but it resulted in less maintainable code.

        var startFallback = false
        val startMatch = BinarySearch.array(
          matching,
          (mt: MatchingToken[A], i) => {
            val result = compare(mt.original.pos, pos.start)
            result match {
              case BinarySearch.Smaller =>
                matchingTokens(i + 1) match {
                  case Some(next) =>
                    compare(next.original.pos, pos.start) match {
                      case BinarySearch.Greater =>
                        startFallback = true
                        // The original token is not available in the revised document
                        // so we use the nearest token instead.
                        BinarySearch.Equal
                      case _ =>
                        result
                    }
                  case None =>
                    startFallback = true
                    BinarySearch.Equal
                }
              case _ =>
                result
            }
          },
        )

        var endFallback = false
        val endMatch = BinarySearch.array(
          matching,
          (mt: MatchingToken[A], i) => {
            // End offsets are non-inclusive so we decrement by one.
            val offset = math.max(pos.start, pos.end - 1)
            val result = compare(mt.original.pos, offset)
            result match {
              case BinarySearch.Greater =>
                matchingTokens(i - 1) match {
                  case Some(next) =>
                    compare(next.original.pos, offset) match {
                      case BinarySearch.Smaller =>
                        endFallback = true
                        BinarySearch.Equal
                      case _ =>
                        result
                    }
                  case None =>
                    endFallback = true
                    BinarySearch.Equal
                }
              case _ =>
                result
            }
          },
        )

        (startMatch, endMatch) match {
          case (Some(start), Some(end)) =>
            if ((startFallback || endFallback) && !fallbackToNearest)
              None
            else if (startFallback && endFallback) {
              val offset = end.revised.start
              Some(Position.Range(revisedInput, offset - 1, offset).toLsp)
            } else if (start.revised == end.revised) {
              // new range spans one token
              if (adjustWithinToken)
                Some(computeAdjustmentWithinToken(range, start).toLsp)
              else Some(start.revised.pos.toLsp)
            } else {
              val endOffset = end.revised match {
                case t if t.isLF => t.start
                case t => t.end
              }
              Some(
                Position
                  .Range(revisedInput, start.revised.start, endOffset)
                  .toLsp
              )
            }
          case (start, end) =>
            logger.warning(
              s"stale range: ${start.map(_.show)} ${end.map(_.show)}"
            )
            None
        }
      }
    }
    def toRevised(
        originalLine: Int,
        originalColumn: Int,
    ): Either[EmptyResult, Position] = {
      toRevised(originalInput.toOffset(originalLine, originalColumn))
    }

    def toRevised(originalOffset: Int): Either[EmptyResult, Position] = {
      BinarySearch
        .array[MatchingToken[A]](
          matching,
          (mt, _) => compare(mt.original.pos, originalOffset),
        )
        .fold(EmptyResult.noMatch)(m => Right(m.revised.pos))
    }

    def toOriginal(
        revisedLine: Int,
        revisedColumn: Int,
    ): Either[EmptyResult, Position] =
      toOriginal(revisedInput.toOffset(revisedLine, revisedColumn))

    def toOriginal(revisedOffset: Int): Either[EmptyResult, Position] =
      BinarySearch
        .array[MatchingToken[A]](
          matching,
          (mt, _) => compare(mt.revised.pos, revisedOffset),
        )
        .fold(EmptyResult.noMatch)(m => Right(m.original.pos))

    private def compare(
        pos: Position,
        offset: Int,
    ): BinarySearch.ComparisonResult =
      if (pos.contains(offset)) BinarySearch.Equal
      else if (pos.end <= offset) BinarySearch.Smaller
      else BinarySearch.Greater

    private def computeAdjustmentWithinToken(
        original: l.Range,
        matching: MatchingToken[A],
    ): Position.Range = {
      val originalStartPositionOpt: Option[Position] =
        original.getStart().toMeta(originalInput)
      val originalEndPositionOpt: Option[Position] =
        original.getEnd().toMeta(originalInput)

      // Original start range - start of the found token
      val startCorrection =
        originalStartPositionOpt.fold(0)(_.start - matching.original.start)
      val endCorrection =
        originalEndPositionOpt.fold(0)(matching.original.end - _.end)

      Position.Range(
        revisedInput,
        matching.revised.start + startCorrection,
        matching.revised.end - endCorrection,
      )
    }

    override def toString(): String = s"Diff(${matching.length} tokens)"
  }

  implicit class XtensionPositionRangeLSP(val pos: Position) extends AnyVal {
    def contains(offset: Int): Boolean =
      if (pos.start == pos.end) pos.end == offset
      else {
        pos.start <= offset &&
        pos.end > offset
      }
  }

  /**
   * Build utility to map offsets between two slightly different strings.
   *
   * @param original The original snapshot of a string, for example the latest
   *                 semanticdb snapshot.
   * @param revised The current snapshot of a string, for example open buffer
   *                in an editor.
   */
  private def fromTokens[A: ClassTag](
      originalInput: Input.VirtualFile,
      original: Array[A],
      revisedInput: Input.VirtualFile,
      revised: Array[A],
  )(implicit ops: TokenOps[A]) = {
    val buffer = Array.newBuilder[MatchingToken[A]]
    buffer.sizeHint(math.max(original.length, revised.length))
    @tailrec
    def loop(
        i: Int,
        j: Int,
        ds: List[Delta[A]],
    ): Unit = {
      val isDone: Boolean =
        i >= original.length ||
          j >= revised.length
      if (isDone) ()
      else {
        val o = original(i)
        val r = revised(j)
        if (ops.equalizer.equals(o, r)) {
          buffer += new MatchingToken(o, r)
          loop(i + 1, j + 1, ds)
        } else {
          ds match {
            case Nil =>
              loop(i + 1, j + 1, ds)
            case delta :: tail =>
              loop(
                i + delta.getOriginal.size(),
                j + delta.getRevised.size(),
                tail,
              )
          }
        }
      }
    }
    val deltas = {
      DiffUtils
        .diff(
          ArraySeq(original: _*).asJava,
          ArraySeq(revised: _*).asJava,
          ops.equalizer,
        )
        .getDeltas
        .iterator()
        .asScala
        .toList
    }
    loop(0, 0, deltas)
    new Diff(buffer.result(), originalInput, revisedInput)
  }

  def apply(
      originalInput: Input.VirtualFile,
      revisedInput: Input.VirtualFile,
      scalaVersionSelector: ScalaVersionSelector,
  ): Either[String, TokenEditDistance] = {
    val isScala =
      originalInput.path.isScalaFilename &&
        revisedInput.path.isScalaFilename
    val isJava =
      originalInput.path.isJavaFilename &&
        revisedInput.path.isJavaFilename

    if (!isScala && !isJava) {
      // Ignore non-scala/java Files.
      Right(Unchanged)
    } else if (originalInput.value.isEmpty() || revisedInput.value.isEmpty()) {
      Right(NoMatch)
    } else if (originalInput == revisedInput) {
      Right(Unchanged)
    } else if (isJava) {
      val tokenizedRevised = JavaTokens.tokenize(revisedInput)
      val tokenizedOriginal = JavaTokens.tokenize(originalInput)
      (tokenizedRevised, tokenizedOriginal) match {
        case (Success(revised), Success(original)) =>
          Right(
            TokenEditDistance.fromTokens(
              originalInput,
              original,
              revisedInput,
              revised,
            )
          )
        case (err: Failure[_], _) => Left(err.exception.getMessage())
        case (_, err: Failure[_]) => Left(err.exception.getMessage())
      }
    } else {
      val path = revisedInput.path.toAbsolutePath
      val dialect = scalaVersionSelector.getDialect(path)
      val tokenizedRevised = revisedInput.value.safeTokenize(dialect)
      val tokenizedOriginal = originalInput.value.safeTokenize(dialect)
      val result = (tokenizedRevised, tokenizedOriginal) match {
        case (Tokenized.Success(revised), Tokenized.Success(original)) =>
          Right(
            TokenEditDistance.fromTokens(
              originalInput,
              original.tokens,
              revisedInput,
              revised.tokens,
            )
          )
        case (err: Tokenized.Error, _) =>
          Left(err.message)
        case (_, err: Tokenized.Error) =>
          Left(err.message)
        case _ => Right(NoMatch)
      }

      result match {
        case Left(message) =>
          scribe.debug(
            s"Could not tokenize file ${revisedInput.path} because of:",
            message,
          )
        case _ =>
      }

      result
    }
  }

}
