package scala.meta.internal.pc

import scala.meta.pc.PresentationCompilerConfig
import java.util.concurrent.TimeUnit;
import java.util
import java.util.Optional
import scala.meta.pc.PresentationCompilerConfig.OverrideDefFormat

case class PresentationCompilerConfigImpl(
    debug: Boolean = false,
    parameterHintsCommand: Optional[String] = Optional.empty,
    completionCommand: Optional[String] = Optional.empty,
    symbolPrefixes: util.Map[String, String] =
      PresentationCompilerConfig.defaultSymbolPrefixes(),
    overrideDefFormat: OverrideDefFormat = OverrideDefFormat.Ascii,
    isCompletionItemDetailEnabled: Boolean = true,
    isCompletionItemDocumentationEnabled: Boolean = true,
    isHoverDocumentationEnabled: Boolean = true,
    snippetAutoIndent: Boolean = true,
    isFoldOnlyLines: Boolean = false,
    isSignatureHelpDocumentationEnabled: Boolean = true,
    isCompletionSnippetsEnabled: Boolean = true,
    isCompletionItemResolve: Boolean = true,
    timeoutDelay: Long = 20,
    timeoutUnit: TimeUnit = TimeUnit.SECONDS
) extends PresentationCompilerConfig
