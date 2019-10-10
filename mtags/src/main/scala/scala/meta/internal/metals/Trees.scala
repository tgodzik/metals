package scala.meta.internal.metals

import scala.collection.concurrent.TrieMap
import scala.meta._
import scala.meta.internal.io.PathIO
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.parsers.Parsed
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity

/**
 * Manages parsing of Scala source files into Scalameta syntax trees.
 *
 * - provides the latest good Scalameta tree for a given source file
 *   similar as `Buffers` provides the current text content.
 * - publishes diagnostics for syntax errors.
 */
final class Trees() {

  private val trees = TrieMap.empty[AbsolutePath, Tree]

  def get(path: AbsolutePath): Option[Tree] =
    trees.get(path).orElse {
      // Fallback to parse without caching result.
      parse(path, path.readText).flatMap(_.toOption)
    }

  def didClose(path: AbsolutePath, text: String): Unit = {
    trees.remove(path)
  }

  def didChange(path: AbsolutePath, text: String): List[Diagnostic] = {
    parse(path, text) match {
      case Some(parsed) =>
        parsed match {
          case Parsed.Error(pos, message, _) =>
            List(
              new Diagnostic(
                pos.toLSP,
                message,
                DiagnosticSeverity.Error,
                "scalameta"
              )
            )
          case Parsed.Success(tree) =>
            trees(path) = tree
            List()
        }
      case None =>
        () // Unknown extension, do nothing.
        List()
    }
  }

  private def parse(
      path: AbsolutePath,
      text: String
  ): Option[Parsed[Source]] = {
    dialect(path).map { d =>
      val input = Input.VirtualFile(path.toString(), text)
      d(input).parse[Source]
    }
  }
  private def dialect(path: AbsolutePath): Option[Dialect] = {
    Option(PathIO.extension(path.toNIO)).collect {
      case "scala" => dialects.Scala
      case "sbt" => dialects.Sbt
      case "sc" => dialects.Sbt
    }
  }
}
