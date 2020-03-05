package tests

import coursier._
import java.nio.file.Files
import java.nio.file.Path
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.ClasspathSearch
import scala.meta.internal.metals.JdkSources
import scala.meta.internal.metals.Docstrings
import scala.meta.internal.metals.RecursivelyDelete
import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.io.AbsolutePath
import scala.meta.pc.PresentationCompilerConfig
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import scala.collection.Seq
import scala.meta.pc.PresentationCompiler
import scala.meta.internal.metals.Embedded
import scala.meta.internal.metals.ScalaVersions
import ch.epfl.scala.bsp4j.ScalaBuildTarget
import ch.epfl.scala.bsp4j.ScalacOptionsItem
import ch.epfl.scala.bsp4j.ScalaPlatform
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import coursier.Fetch
import coursier.core.Classifier
import scala.meta.internal.mtags.ClasspathLoader
import java.nio.file.Paths
import scala.meta.internal.mtags.GlobalSymbolIndex
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal
import munit.TestOptions

case class TestEnvironment(
    pc: PresentationCompiler,
    index: GlobalSymbolIndex,
    workspace: TestingWorkspaceSearch,
    scalaVersion: String
)

abstract class BasePCSuite extends BaseSuite {

  val executorService: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor()

  val presentationCompilers: Map[String, TestEnvironment] =
    BuildInfo.supportedScalaVersions
      .filterNot(excludedScalaVersions.contains)
      .map(scalaVersion => scalaVersion -> newPresentationCompiler(scalaVersion)
      )
      .toMap

  def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl().copy(
      snippetAutoIndent = false
    )

  def extraClasspath: Seq[Path] = Nil
  def scalacOptions: Seq[String] = Nil
  def excludedScalaVersions: Set[String] = Set.empty

  protected def newIndex =
    new DelegatingGlobalSymbolIndex(OnDemandSymbolIndex())

  private def indexScalaLibrary(
      index: GlobalSymbolIndex,
      scalaVersion: String
  ): Unit = {
    val sources = Fetch()
      .addClassifiers(Classifier.sources)
      .addDependencies(
        coursier.Dependency(
          mod"org.scala-lang:scala-library",
          // NOTE(gabro): we should ideally just use BuildoInfoVersions.scalaVersion
          // but using the 2.11 stdlib would cause a lot tests to break for little benefit.
          // We can remove this switch once we drop support for 2.11
          scalaVersion match {
            case v if v.startsWith("2.13") => v
            case v if v.startsWith("2.12") => v
            case _ => BuildInfoVersions.scala212
          }
        )
      )
      .run()
    sources.foreach { jar =>
      index.addSourceJar(AbsolutePath(jar))
    }
  }

  private def newPresentationCompiler(scalaVersion: String): TestEnvironment = {

    val binaryVersion = ScalaVersions.toBinaryVersion(scalaVersion)
    val scalaDependencies =
      if (ScalaVersions.isScala3Version(scalaVersion))
        ScalaDependencies.scala3(scalaVersion)
      else
        ScalaDependencies.scala2(scalaVersion)

    val scalaLibrary: Seq[Path] = Fetch()
      .addDependencies(scalaDependencies: _*)
      .run()
      .map(_.toPath())

    val myclasspath: Seq[Path] = (extraClasspath ++ scalaLibrary).filterNot {
      path =>
        val filename = path.toString()
        filename.contains("scala-reflect") ||
        filename.contains("scala-compiler") ||
        filename.contains("scala-xml")
    }

    val index = newIndex
    if (requiresJdkSources)
      JdkSources().foreach(jdk => index.addSourceJar(jdk))
    if (requiresScalaLibrarySources)
      indexScalaLibrary(index, scalaVersion)
    val indexer = new Docstrings(index)
    val workspace = new TestingWorkspaceSearch
    val search = new TestingSymbolSearch(
      ClasspathSearch.fromClasspath(myclasspath),
      new Docstrings(index),
      workspace,
      index
    )

    val info: ScalaBuildTarget = new ScalaBuildTarget(
      "org.scala-lang",
      scalaVersion,
      ScalaVersions.toBinaryVersion(scalaVersion),
      ScalaPlatform.JVM,
      myclasspath.map(_.toUri().toString()).asJava
    )

    val scalac = new ScalacOptionsItem(
      new BuildTargetIdentifier("root"),
      scalacOptions.asJava,
      scalaLibrary.map(_.toUri().toString()).asJava,
      ""
    )

    val classloader =
      if (ScalaVersions.isScala3Version(scalaVersion)) {
        Embedded.newScala3PresentationCompilerClassLoader(info, scalac)
      } else {
        Embedded.newPresentationCompilerClassLoader(info, scalac)
      }

    val pc = Embedded
      .serviceLoader(
        classOf[PresentationCompiler],
        classOf[ScalaPresentationCompiler].getName(),
        classloader
      )
      .withSearch(search)
      .withConfiguration(config)
      .withExecutorService(executorService)
      .withScheduledExecutorService(executorService)
      .newInstance("", myclasspath.asJava, scalacOptions.asJava)
    TestEnvironment(pc, index, workspace, scalaVersion)
  }

  def thisClasspath: Seq[Path] =
    ClasspathLoader
      .getURLs(this.getClass.getClassLoader)
      .map(url => Paths.get(url.toURI))

  val tmp: AbsolutePath = AbsolutePath(Files.createTempDirectory("metals"))

  override def afterAll(): Unit = {
    presentationCompilers.foreach {
      case (_, env) =>
        env.pc.shutdown()
    }
    RecursivelyDelete(tmp)
    executorService.shutdown()
  }

  def testPc(name: TestOptions, ignoredScalaVersions: Set[String] = Set.empty)(
      fn: TestEnvironment => Unit
  ) = {
    presentationCompilers.foreach {
      case (scalaVersion, env) =>
        val testOptions: TestOptions =
          name.withName(s"${name.name}_$scalaVersion")
        val possiblyIgnored =
          if (ignoredScalaVersions(scalaVersion)) testOptions.ignore
          else testOptions
        test(possiblyIgnored) {
          fn(env)
        }

    }
  }

  def requiresJdkSources: Boolean = false
  def requiresScalaLibrarySources: Boolean = false

  def params(code: String, filename: String = "test.scala")(
      implicit testEnvironment: TestEnvironment
  ): (String, Int) = {
    val code2 = code.replaceAllLiterally("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }
    val file = tmp.resolve(filename)
    Files.write(file.toNIO, code2.getBytes(StandardCharsets.UTF_8))
    try testEnvironment.index.addSourceFile(file, Some(tmp))
    catch {
      case NonFatal(e) =>
        println(s"warn: $e")
    }
    testEnvironment.workspace.inputs(filename) = code2
    (code2, offset)
  }

  def doc(e: JEither[String, MarkupContent]): String = {
    if (e == null) ""
    else if (e.isLeft) {
      " " + e.getLeft
    } else {
      " " + e.getRight.getValue
    }
  }.trim

  def sortLines(stableOrder: Boolean, string: String): String = {
    if (stableOrder) string
    else string.linesIterator.toList.sorted.mkString("\n")
  }
}

object ScalaDependencies {

  def scala3(scalaVersion: String) = {
    val binaryVersion = ScalaVersions.toBinaryVersion(scalaVersion)
    Seq(
      Dependency(mod"org.scala-lang:scala-library", "2.13.1"),
      Dependency(
        Module(
          Organization("ch.epfl.lamp"),
          ModuleName(s"dotty-compiler_$binaryVersion")
        ),
        scalaVersion
      ),
      Dependency(
        Module(
          Organization("ch.epfl.lamp"),
          ModuleName(s"tasty-core_$binaryVersion")
        ),
        scalaVersion
      ),
      Dependency(
        Module(
          Organization("ch.epfl.lamp"),
          ModuleName(s"dotty-library_$binaryVersion")
        ),
        scalaVersion
      ),
      Dependency(mod"ch.epfl.lamp:dotty-interfaces", scalaVersion)
    )
  }
  def scala2(scalaVersion: String) = {
    Seq(
      Dependency(mod"org.scala-lang:scala-library", scalaVersion),
      Dependency(mod"org.scala-lang:scala-compiler", scalaVersion),
      Dependency(mod"org.scala-lang:scala-reflect", scalaVersion)
    )
  }

}
