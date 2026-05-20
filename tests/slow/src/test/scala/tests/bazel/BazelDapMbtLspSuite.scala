package tests.bazel

import scala.concurrent.duration._

import scala.meta.internal.builds.ShellRunner
import scala.meta.internal.metals.AutoImportBuildKind
import scala.meta.internal.metals.Configs.JavaSymbolLoaderConfig
import scala.meta.internal.metals.Configs.ReferenceProviderConfig
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.mbt.MbtBuildServer
import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.DebugSessionParamsDataKind
import ch.epfl.scala.bsp4j.ScalaMainClass
import tests.BaseDapSuite
import tests.BazelBuildLayout
import tests.BazelMbtTestInitializer
import tests.TestHovers

/**
 * End-to-end: Bazel workspace → MBT import (`bazel query` + `.metals/mbt.json`)
 * → [[MbtBuildServer]] → Scala hover.
 */
class BazelDapMbtLspSuite
    extends BaseDapSuite(
      "bazel-mbt-dap",
      BazelMbtTestInitializer,
      BazelBuildLayout,
    )
    with TestHovers {

  private val bazelVersion = "8.2.1"

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      fallbackScalaVersion = Some(
        "2.13.12"
      ), // This should not be used if the target has a scala version
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

  private val catsVersion = "2.13.0"
  private val jsoupVersion = "1.21.1"

  /** Same targets as [[BazelLspSuite]], plus a project view so MBT import scopes `bazel query`. */
  private def bazelWorkspaceLayout: String = {
    val projectView =
      """/.bazelproject
        |targets:
        |    //...
        |
        |""".stripMargin
    val rulesAndSources =
      s"""|/core/BUILD
          |load("@rules_scala//scala:scala.bzl", "scala_library")
          |
          |scala_library(
          |    name = "hello_lib",
          |    srcs = ["Hello.scala", "Bar.scala"],
          |    visibility = ["//visibility:public"],
          |    scalacopts = ["-deprecation"],
          |    deps = ["@maven//:org_typelevel_cats_core_2_13"],
          |)
          |
          |/core/Hello.scala
          |package core
          |
          |import cats.syntax.all._
          |
          |class Hello {
          |  def hello: String = "Hello"
          |  def catOption: Option[Int] = 1.some
          |
          |}
          |
          |/core/Bar.scala
          |package core
          |
          |class Bar {
          |  def bar: String = "bar"
          |  def hi = new Hello().hello
          |}
          |
          |/app/BUILD
          |load("@rules_scala//scala:scala.bzl", "scala_binary")
          |
          |scala_binary(
          |    name = "hello",
          |    srcs = ["Main.scala", "Decode.scala"],
          |    main_class = "main",
          |    deps = ["//core:hello_lib"],
          |)
          |
          |/app/Main.scala
          |import core.Hello
          |
          |object Main {
          |  def main(args: Array[String]): Unit = {
          |    println(msg)
          |    System.exit(0)
          |  }
          |  def msg = new Hello().hello
          |}
          |
          |/app/Decode.scala
          |package app
          |
          |class Decode {
          | def decoded = this
          |}
          |
          |object Decode {
          | def decode: String = "decode"
          |}
          |""".stripMargin
    projectView + rulesAndSources
  }

  private val mavenDeps: List[String] = List(
    s"org.typelevel:cats-core_2.13:$catsVersion"
  )

  List(
    s"org.jsoup:jsoup:$jsoupVersion"
  )

  private def pinMaven(workspace: AbsolutePath): Unit = {
    workspace.resolve("maven_install.json").touch()
    ShellRunner.runSync(
      List("bazel", "run", "@maven//:pin"),
      workspace,
      redirectErrorOutput = false,
      timeout = 1.minute,
    )
  }

  test("bazel-import-mbt-server-hover") {
    cleanWorkspace()

    val mainClass = new ScalaMainClass(
      "Main",
      List("Bar").asJava,
      List("-Dproperty=Foo").asJava,
    )
    for {
      _ <- initialize(
        BazelBuildLayout(
          bazelWorkspaceLayout,
          V.scala213,
          bazelVersion,
          mavenDeps,
        ),
        runAdditionalCommands = pinMaven,
      )
      _ <- server.headServer.connectionProvider.buildServerPromise.future
      _ <- server.didOpen("core/Hello.scala")

      debugger <- server.startDebugging(
        "mbt://namespace///app",
        DebugSessionParamsDataKind.SCALA_MAIN_CLASS,
        mainClass,
      )
      _ <- debugger.initialize
      _ <- debugger.launch
      _ <- debugger.configurationDone
      _ <- debugger.shutdown
      output <- debugger.allOutput
    } yield assertNoDiff(output, "FooBarFoo")
  }

}
