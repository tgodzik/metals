package scala.meta.internal.metals

import scala.meta.pc.CancelToken
import scala.meta.pc.OffsetParams

class CompilerOffsetParams(
    val filename: String,
    val text: String,
    val offset: Int,
    val token: CancelToken = EmptyCancelToken
) extends OffsetParams

object CompilerOffsetParams {

  val sbtPrefix = "object A{\n"

  def apply(
      filename: String,
      text: String,
      offset: Int,
      token: CancelToken = EmptyCancelToken
  ): CompilerOffsetParams = {
    val (sbtText, newOffset) = if (filename.endsWith(".sbt")) {
      (sbtPrefix + text + "\n}", offset + sbtPrefix.size)
    } else (text, offset)
    new CompilerOffsetParams(filename, sbtText, newOffset, token)
  }

}
