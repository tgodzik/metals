package tests

import scala.meta.internal.metals.DocumentSymbolProvider
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.{semanticdb => s}
import tests.MetalsTestEnrichments._
import scala.meta.internal.metals.Trees

/**
 * Checks the positions of document symbols inside a document
 */
object DocumentSymbolSuite extends DirectoryExpectSuite("documentSymbol") {
  val documentSymbolProvider = new DocumentSymbolProvider(new Trees())

  override def testCases(): List[ExpectTestCase] = {
    input.scalaFiles.map { file =>
      ExpectTestCase(
        file, { () =>
          val documentSymbols = documentSymbolProvider
            .documentSymbols(file.file.filename, file.code)
            .asScala
          val flatSymbols =
            documentSymbols.toSymbolInformation(file.file.toURI.toString)
          val textDocument = s.TextDocument(
            schema = s.Schema.SEMANTICDB4,
            language = s.Language.SCALA,
            text = file.input.text,
            occurrences = flatSymbols.map(_.toSymbolOccurrence)
          )

          Semanticdbs.printTextDocument(textDocument)
        }
      )
    }
  }

}
