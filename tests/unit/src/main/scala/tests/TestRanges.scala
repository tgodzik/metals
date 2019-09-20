package tests

import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.Location

object TestRanges extends RangeReplace {

  def renderHighlightsAsString(
      code: String,
      highlights: List[DocumentHighlight]
  ): String = {
    highlights.foldLeft(code) { (base, highlight) =>
      replaceInRange(base, highlight.getRange)
    }
  }

  def renderLocationsAsString(
      sourceFiles: Seq[(String, String)],
      locations: List[Location]
  ): Seq[String] = {
    for {
      (file, code) <- sourceFiles
      validLocations = locations.filter(_.getUri().contains(file))
    } yield
      validLocations.foldLeft(code) { (base, highlight) =>
        replaceInRange(base, highlight.getRange)
      }
  }
}
