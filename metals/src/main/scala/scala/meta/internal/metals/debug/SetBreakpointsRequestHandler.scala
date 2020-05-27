package scala.meta.internal.metals.debug
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.JvmSignatures
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence

import org.eclipse.lsp4j.debug.SetBreakpointsArguments
import org.eclipse.lsp4j.debug.SetBreakpointsResponse
import org.eclipse.lsp4j.debug.Source
import org.eclipse.lsp4j.debug.SourceBreakpoint

private[debug] final class SetBreakpointsRequestHandler(
    server: ServerAdapter,
    adapters: MetalsDebugAdapters,
    semanticdbs: Semanticdbs
)(implicit ec: ExecutionContext) {

  def apply(
      request: SetBreakpointsArguments
  ): Future[SetBreakpointsResponse] = {
    val path =
      adapters.adaptPathForServer(request.getSource.getPath).toAbsolutePath

    val originalSource = DebugProtocol.copy(request.getSource)
    def topLevels = Mtags.allToplevels(path.toInput)
    val occurrences = path.toLanguage match {
      case Language.JAVA =>
        // make sure only type symbols are under consideration,
        // as static methods are also included in top-levels
        topLevels.occurrences
      case _ =>
        semanticdbs
          .textDocument(path)
          .documentIncludingStale
          .map(_.occurrences)
          .getOrElse(topLevels.occurrences)
    }

    val groups = request.getBreakpoints.groupBy { breakpoint =>
      val definition = occurrences.minBy(distanceFrom(breakpoint))
      JvmSignatures.toTypeSignature(definition)
    }

    val partitions = groups.map {
      case (fqcn, breakpoints) =>
        createPartition(request, fqcn.value, breakpoints)
    }

    server
      .sendPartitioned(partitions.map(DebugProtocol.syntheticRequest))
      .map(_.map(DebugProtocol.parseResponse[SetBreakpointsResponse]))
      .map(_.flatMap(_.toList))
      .map(assembleResponse(_, originalSource))
  }

  private def assembleResponse(
      responses: Iterable[SetBreakpointsResponse],
      originalSource: Source
  ): SetBreakpointsResponse = {
    val breakpoints = for {
      response <- responses
      breakpoint <- response.getBreakpoints
    } yield {
      breakpoint.setSource(originalSource)
      // note(@tgodzik) this seems to happen from time to time, not exactly sure why
      // can be removed if the issue is closed:
      // https://github.com/scalameta/metals/issues/1569
      if (breakpoint.getSource() == null)
        scribe.warn(
          s"[DAP] Could not set the original source for breakpoint, since it was $originalSource"
        )
      breakpoint
    }

    val response = new SetBreakpointsResponse
    response.setBreakpoints(breakpoints.toArray)
    response
  }

  private def createPartition(
      request: SetBreakpointsArguments,
      fqcn: String,
      breakpoints: Array[SourceBreakpoint]
  ) = {
    val source = DebugProtocol.copy(request.getSource)
    source.setPath(s"dap-fqcn:$fqcn")

    val lines = breakpoints
      .map(_.getLine: Integer)
      .distinct

    val partition = new SetBreakpointsArguments
    partition.setBreakpoints(breakpoints)
    partition.setSource(source)
    partition.setLines(lines)
    partition.setSourceModified(request.getSourceModified)

    partition
  }

  private def distanceFrom(
      breakpoint: SourceBreakpoint
  ): SymbolOccurrence => Long = { occ =>
    if (occ.symbol.isLocal || !occ.role.isDefinition) { Long.MaxValue }
    else {
      val startLine = occ.range.fold(Int.MaxValue)(_.startLine)
      val breakpointLine = adapters.adaptLine(breakpoint.getLine)
      if (startLine > breakpointLine) Long.MaxValue
      else breakpointLine - startLine
    }
  }
}
