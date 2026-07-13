package scala.meta.internal.metals

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import javax.tools.ToolProvider

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Compiles all Java files for a build target using javac + annotation
 * processors and writes class files to a dedicated output directory.
 */
class JavaAnnotationProcessorBatchCompiler(
    classpath: Seq[Path],
    processorOpts: List[String],
    outputDir: Path,
) {

  def compile(javaFiles: Seq[Path]): Boolean =
    if (javaFiles.isEmpty) false
    else
      try {
        Files.createDirectories(outputDir)
        val compiler = ToolProvider.getSystemJavaCompiler()
        if (compiler == null) {
          scribe.warn(
            "[JavaAnnotationProcessorBatchCompiler] no system Java compiler available (running on JRE?)"
          )
          return false
        }
        val fileManager = compiler.getStandardFileManager(null, null, null)
        try {
          val units = fileManager.getJavaFileObjectsFromPaths(javaFiles.asJava)
          val options = buildOptions()
          val task =
            compiler.getTask(
              null,
              fileManager,
              null,
              options.asJava,
              null,
              units,
            )
          task.call()
        } finally {
          fileManager.close()
        }
      } catch {
        case NonFatal(e) =>
          scribe.warn(
            s"[JavaAnnotationProcessorBatchCompiler] compilation failed: ${e.getMessage}"
          )
          false
      }

  private def buildOptions(): List[String] = {
    val opts = List.newBuilder[String]
    opts += "-d"
    opts += outputDir.toString()
    if (classpath.nonEmpty) {
      opts += "-classpath"
      opts += classpath.mkString(File.pathSeparator)
    }
    opts ++= processorOpts
    opts.result()
  }
}
