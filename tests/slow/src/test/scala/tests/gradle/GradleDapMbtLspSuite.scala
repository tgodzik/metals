package tests.gradle

import scala.jdk.CollectionConverters._

import scala.meta.internal.metals.AutoImportBuildKind
import scala.meta.internal.metals.Configs.JavaSymbolLoaderConfig
import scala.meta.internal.metals.Configs.ReferenceProviderConfig
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.mbt.MbtBuildServer

import ch.epfl.scala.bsp4j.DebugSessionParamsDataKind
import ch.epfl.scala.bsp4j.ScalaMainClass
import tests.BaseDapSuite
import tests.QuickBuildInitializer
import tests.QuickBuildLayout

class GradleDapMbtLspSuite
    extends BaseDapSuite(
      "gradle-mbt-dap",
      QuickBuildInitializer,
      QuickBuildLayout,
    ) {

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      presentationCompilerDiagnostics = true,
      buildOnChange = false,
      buildOnFocus = false,
      workspaceSymbolProvider = WorkspaceSymbolProviderConfig.mbt,
      javaSymbolLoader = JavaSymbolLoaderConfig.turbineClasspath,
      referenceProvider = ReferenceProviderConfig.mbt,
      preferredBuildServer = Some(MbtBuildServer.name),
      automaticImportBuild = AutoImportBuildKind.All,
    )

  override def initializeGitRepo: Boolean = true

  private def buildGradle: String =
    """|plugins {
       |    id 'java'
       |}
       |repositories {
       |    mavenCentral()
       |}
       |""".stripMargin

  test("gradle-mbt-debug-session") {
    client.selectedServer = Messages.ChooseBuildServer.mbt
    cleanWorkspace()

    val mainClass = new ScalaMainClass(
      "a.Main",
      List("Bar").asJava,
      List("-Dproperty=Foo").asJava,
    )

    for {
      _ <- initialize(
        s"""|/build.gradle
            |$buildGradle
            |/src/main/java/a/Main.java
            |package a;
            |
            |public class Main {
            |  public static void main(String[] args) {
            |    String foo = System.getProperty("property", "");
            |    String bar = args.length > 0 ? args[0] : "";
            |    System.out.print(foo + bar);
            |    System.exit(0);
            |  }
            |}
            |""".stripMargin
      )
      _ <- server.headServer.connectionProvider.buildServerPromise.future
      _ <- server.didOpen("src/main/java/a/Main.java")
      debugger <- server.startDebugging(
        "gradle-mbt-debug-session",
        DebugSessionParamsDataKind.SCALA_MAIN_CLASS,
        mainClass,
      )
      _ <- debugger.initialize
      _ <- debugger.launch
      _ <- debugger.configurationDone
      _ <- debugger.shutdown
      output <- debugger.allOutput
    } yield assertNoDiff(output, "FooBar")
  }
}
