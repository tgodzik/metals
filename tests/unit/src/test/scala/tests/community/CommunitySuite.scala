package tests.community

import tests.BaseLspSuite
import scala.meta.internal.builds.ShellRunner
import scala.concurrent.Future
import scala.meta.internal.metals.ServerCommands

class CommunitySuite extends BaseLspSuite("community", null) {

  def initializeRepo(repo: String) = Future {
    // TODO checkout specific hash
    ShellRunner.runSync(
      List("git", "clone", "--depth", "1", repo, workspace.toString()),
      workspace,
      redirectErrorOutput = false,
    )
    for {
      _ <- server.initialize(None)
      _ <- server.initialized()
      _ <- server.didChangeConfiguration(userConfig.toString)
    } yield ()
  }
  test("zio".ignore) {
    val problematicFile =
      "zio-http/jvm/src/main/scala/zio/http/netty/client/NettyClientDriver.scala"
    for {
      _ <- initializeRepo("https://github.com/zio/zio-http")
      _ <- server.executeCommand(ServerCommands.ImportBuild)
      _ <- server.server.indexingPromise.future
      _ <- server.didOpen(problematicFile)
      _ <- server.didChange(problematicFile) { text =>
        text.replace(
          "  val live: URLayer[EventLoopGroups.Config, ClientDriver] =",
          "  val live =",
        )
      }
      _ <- server.didSave(problematicFile)(identity)
      _ <- server.assertHoverAtLine(
        problematicFile,
        "  val live@@ =",
        "something",
      )
    } yield {}
  }

}
