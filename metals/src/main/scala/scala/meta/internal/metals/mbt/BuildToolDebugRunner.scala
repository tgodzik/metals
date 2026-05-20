package scala.meta.internal.metals.mbt

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.process.SystemProcess
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.DebugSessionAddress

/**
 * Trait for running build tools in debug mode.
 * Implementations should start the build tool with the JVM debug agent
 * and return the debug port once the process is listening.
 */
trait BuildToolDebugRunner {

  /**
   * Start the build tool in debug mode for the given main class.
   *
   * @param workspace the workspace root
   * @param mainClass the fully qualified main class name to run
   * @param args arguments to pass to the main class
   * @param jvmOptions JVM options (the debug agent will be added automatically)
   * @return a Future containing the DebugSessionAddress once the process is listening
   */
  def startDebugSession(
      workspace: AbsolutePath,
      mainClass: String,
      args: List[String],
      jvmOptions: List[String],
  )(implicit ec: ExecutionContext): Future[DebugSessionAddress]

  /**
   * Cancel any running debug session.
   */
  def cancel(): Unit
}

object BuildToolDebugRunner {
  val JDINotificationPrefix = "Listening for transport dt_socket at address: "
  val DebugAgentFlag =
    "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n"

  /**
   * Parse the debug port from a JVM debug agent output line.
   * Returns None if the line doesn't contain the expected prefix.
   */
  def parseDebugPort(line: String): Option[Int] =
    if (line.startsWith(JDINotificationPrefix)) {
      try {
        Some(Integer.parseInt(line.drop(JDINotificationPrefix.length).trim))
      } catch {
        case _: NumberFormatException => None
      }
    } else None
}

/**
 * Gradle implementation of BuildToolDebugRunner.
 * Runs `gradle run` with the debug agent attached.
 */
class GradleBuildToolRunner extends BuildToolDebugRunner {
  import BuildToolDebugRunner._

  @volatile private var currentProcess: Option[SystemProcess] = None

  override def startDebugSession(
      workspace: AbsolutePath,
      mainClass: String,
      args: List[String],
      jvmOptions: List[String],
  )(implicit ec: ExecutionContext): Future[DebugSessionAddress] = {
    val portPromise = Promise[Int]()

    val allJvmOptions = DebugAgentFlag :: jvmOptions
    val jvmArgsString = allJvmOptions.mkString(" ")

    val gradleWrapper =
      if (workspace.resolve("gradlew").exists) "./gradlew"
      else "gradle"

    val mainClassArg =
      if (mainClass.nonEmpty) List(s"--args=$mainClass") else Nil
    val argsOption =
      if (args.nonEmpty) List(s"--args=${args.mkString(" ")}") else Nil

    val cmd = List(
      gradleWrapper,
      "run",
      s"-Dorg.gradle.jvmargs=$jvmArgsString",
    ) ::: mainClassArg ::: argsOption

    scribe.info(
      s"Starting Gradle debug session with command: ${cmd.mkString(" ")}"
    )

    val process = SystemProcess.run(
      cmd = cmd,
      cwd = workspace,
      redirectErrorOutput = false,
      env = Map.empty,
      processOut = Some { line =>
        scribe.debug(s"Gradle stdout: $line")
        parseDebugPort(line).foreach { port =>
          portPromise.trySuccess(port)
        }
      },
      processErr = Some { line =>
        scribe.debug(s"Gradle stderr: $line")
        parseDebugPort(line).foreach { port =>
          portPromise.trySuccess(port)
        }
      },
    )

    currentProcess = Some(process)

    process.complete.foreach { exitCode =>
      if (!portPromise.isCompleted) {
        portPromise.tryFailure(
          new RuntimeException(
            s"Gradle process exited with code $exitCode before debug port was available"
          )
        )
      }
      currentProcess = None
    }

    portPromise.future.map { port =>
      new DebugSessionAddress(s"tcp://127.0.0.1:$port")
    }
  }

  override def cancel(): Unit =
    currentProcess.foreach(_.cancel)
}

/**
 * Placeholder for Bazel implementation.
 */
class BazelBuildToolRunner extends BuildToolDebugRunner {
  override def startDebugSession(
      workspace: AbsolutePath,
      mainClass: String,
      args: List[String],
      jvmOptions: List[String],
  )(implicit ec: ExecutionContext): Future[DebugSessionAddress] =
    Future.failed(
      new UnsupportedOperationException(
        "Bazel debug runner not yet implemented"
      )
    )

  override def cancel(): Unit = ()
}

/**
 * Placeholder for Maven implementation.
 */
class MavenBuildToolRunner extends BuildToolDebugRunner {
  override def startDebugSession(
      workspace: AbsolutePath,
      mainClass: String,
      args: List[String],
      jvmOptions: List[String],
  )(implicit ec: ExecutionContext): Future[DebugSessionAddress] =
    Future.failed(
      new UnsupportedOperationException(
        "Maven debug runner not yet implemented"
      )
    )

  override def cancel(): Unit = ()
}

/**
 * Placeholder for sbt implementation.
 */
class SbtBuildToolRunner extends BuildToolDebugRunner {
  override def startDebugSession(
      workspace: AbsolutePath,
      mainClass: String,
      args: List[String],
      jvmOptions: List[String],
  )(implicit ec: ExecutionContext): Future[DebugSessionAddress] =
    Future.failed(
      new UnsupportedOperationException("sbt debug runner not yet implemented")
    )

  override def cancel(): Unit = ()
}
