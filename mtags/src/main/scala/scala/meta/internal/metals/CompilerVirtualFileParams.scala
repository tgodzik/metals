package scala.meta.internal.metals

import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams

case class CompilerVirtualFileParams(
    filename: String,
    text: String,
    token: CancelToken = EmptyCancelToken
) extends VirtualFileParams
