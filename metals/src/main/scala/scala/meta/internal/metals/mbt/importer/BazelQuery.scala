package scala.meta.internal.metals.mbt.importer

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.builds.BazelProjectViewTargets
import scala.meta.internal.builds.ShellRunner
import scala.meta.internal.process.ExitCodes
import scala.meta.io.AbsolutePath

object BazelQuery {
  case class Env(
      projectRoot: AbsolutePath,
      shellRunner: ShellRunner,
      javaHome: Option[String],
  )

  def buildRuleKindsQuery(patterns: List[String]): BazelQuery = {
    val ps =
      if (patterns.isEmpty) BazelProjectViewTargets.defaultPatterns
      else patterns
    val parts = for {
      k <- ruleKinds
      p <- ps
    } yield s"kind($k, $p)"
    val query = parts.mkString(" union ")
    BazelQuery(query, outputMode = "label")
  }

  def fullInformationQuery(targets: List[String]): BazelQuery = {
    val query = s"set(${targets.mkString(" ")})"
    BazelQuery(query, outputMode = "xml")
  }

  def allScalaLibrariesQuery: BazelQuery =
    BazelQuery("filter('scala.library', deps(//...))", outputMode = "label")

  private val ruleKinds: List[String] =
    List(
      "scala_library", "java_library", "scala_binary", "java_binary",
      "scala_test", "java_test",
    )

}

case class BazelQuery(
    query: String,
    // Allowed values: "label", "xml"
    // TODO replace with a case class or enum
    outputMode: String,
) {
  import BazelQuery.Env
  def output(outputMode: String): BazelQuery =
    this.copy(outputMode = outputMode)

  def run(
      env: Env
  )(implicit ec: ExecutionContext): Future[String] = {
    import env._
    val buf = new StringBuilder()
    shellRunner
      .run(
        "bazel-mbt-query",
        List(
          "bazel",
          "query",
          query,
          s"--output=$outputMode",
          "--keep_going",
        ),
        projectRoot,
        redirectErrorOutput = false,
        javaHome,
        processOut = line => {
          buf.append(line)
          buf.append(System.lineSeparator())
        },
        processErr = scribe.warn(_),
      )
      .future
      .flatMap {
        case ExitCodes.Success =>
          Future.successful(buf.toString)
        case ExitCodes.Cancel =>
          Future.failed(
            new java.util.concurrent.CancellationException(
              "bazel-mbt: query cancelled"
            )
          )
        case code =>
          Future.failed(
            new Exception(s"bazel-mbt: bazel query failed with exit code $code")
          )
      }
  }

}
