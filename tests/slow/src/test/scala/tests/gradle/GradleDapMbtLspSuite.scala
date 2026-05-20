package tests.gradle

import scala.meta.internal.metals.AutoImportBuildKind
import scala.meta.internal.metals.Configs.JavaSymbolLoaderConfig
import scala.meta.internal.metals.Configs.ReferenceProviderConfig
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.mbt.MbtBuildServer
import scala.meta.internal.metals.{BuildInfo => V}

import ch.epfl.scala.bsp4j.DebugSessionParamsDataKind
import ch.epfl.scala.bsp4j.ScalaMainClass
import tests.BaseDapSuite
import tests.GradleBuildLayout
import tests.GradleMbtTestInitializer

/**
 * End-to-end: Gradle workspace → MBT import → [[MbtBuildServer]] → Debug session.
 */
class GradleDapMbtLspSuite
    extends BaseDapSuite(
      "gradle-mbt-dap",
      GradleMbtTestInitializer,
      GradleBuildLayout,
    ) {

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      fallbackScalaVersion = Some(V.scala213),
      presentationCompilerDiagnostics = true,
      buildOnChange = false,
      buildOnFocus = false,
      workspaceSymbolProvider = WorkspaceSymbolProviderConfig.mbt,
      javaSymbolLoader = JavaSymbolLoaderConfig.turbineClasspath,
      referenceProvider = ReferenceProviderConfig.mbt,
      preferredBuildServer = Some(MbtBuildServer.name),
      automaticImportBuild = AutoImportBuildKind.All,
    )

  private def gradleWorkspaceLayout: String =
    s"""|/build.gradle
        |plugins {
        |    id 'scala'
        |    id 'application'
        |}
        |repositories {
        |    mavenCentral()
        |}
        |dependencies {
        |    implementation 'org.scala-lang:scala-library:${V.scala213}'
        |}
        |application {
        |    mainClass = 'Main'
        |}
        |
        |/src/main/scala/Hello.scala
        |class Hello {
        |  def hello: String = "Hello"
        |}
        |
        |/src/main/scala/Main.scala
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    val hello = new Hello()
        |    println(hello.hello)
        |    args.foreach(println)
        |    System.exit(0)
        |  }
        |}
        |""".stripMargin

  test("gradle-mbt-debug-session") {
    cleanWorkspace()

    val mainClass = new ScalaMainClass(
      "Main",
      List("World").asJava,
      List.empty[String].asJava,
    )

    for {
      _ <- initialize(gradleWorkspaceLayout)
      _ <- server.headServer.connectionProvider.buildServerPromise.future
      _ <- server.didOpen("src/main/scala/Main.scala")
      debugger <- server.startDebugging(
        "mbt://namespace/default",
        DebugSessionParamsDataKind.SCALA_MAIN_CLASS,
        mainClass,
      )
      _ <- debugger.initialize
      _ <- debugger.launch
      _ <- debugger.configurationDone
      _ <- debugger.shutdown
      output <- debugger.allOutput
    } yield assertNoDiff(
      output,
      """|Hello
         |World""".stripMargin,
    )
  }
}
