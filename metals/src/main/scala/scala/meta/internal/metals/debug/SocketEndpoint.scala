package scala.meta.internal.metals.debug

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.StringReader
import java.net.Socket
import java.util.Collections

import scala.collection.concurrent.TrieMap

import com.google.gson.stream.JsonReader
import org.eclipse.lsp4j.debug.services.IDebugProtocolServer
import org.eclipse.lsp4j.jsonrpc.MessageConsumer
import org.eclipse.lsp4j.jsonrpc.debug.json.DebugMessageJsonHandler
import org.eclipse.lsp4j.jsonrpc.debug.messages.DebugResponseMessage
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler
import org.eclipse.lsp4j.jsonrpc.json.MethodProvider
import org.eclipse.lsp4j.jsonrpc.json.StreamMessageConsumer
import org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer
import org.eclipse.lsp4j.jsonrpc.messages.Message
import org.eclipse.lsp4j.jsonrpc.messages.RequestMessage
import org.eclipse.lsp4j.jsonrpc.messages.ResponseMessage
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints

private[debug] final class SocketEndpoint(socket: Socket)
    extends RemoteEndpoint {
  private val methods =
    ServiceEndpoints.getSupportedMethods(classOf[IDebugProtocolServer])
  private val handler = new DebugMessageJsonHandler(methods)
  private val source = messageSource(socket)
  private val target = messageTarget(socket)
  private val ongoing = TrieMap.empty[String, String]

  override def consume(message: Message): Unit = {
    message match {
      case msg: RequestMessage =>
        ongoing.put(msg.getId(), msg.getMethod())
      case _ =>
    }
    target.consume(message)
  }

  override def listen(consumer: MessageConsumer): Unit = {
    source.listen(consumer)
  }

  override def cancel(): Unit = {
    source.close()
    socket.close()
  }

  handler.setMethodProvider {
    new MethodProvider {
      def resolveMethod(requestId: String): String = {
        val result = ongoing.get(requestId).getOrElse {
          throw new RuntimeException(s"Unknown request $requestId")
        }
        ongoing.remove(requestId)
        result
      }
    }
  }

  private def messageSource(socket: Socket): StreamMessageProducer = {
    val stream = new BufferedInputStream(socket.getInputStream)
    new StreamMessageProducer(stream, handler)
  }

  private def messageTarget(socket: Socket): MessageConsumer = {
    val stream = new BufferedOutputStream(socket.getOutputStream)
    new StreamMessageConsumer(stream, handler)
  }
}
