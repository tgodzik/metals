package scala.meta.internal.builds.bazelnative

import java.net.URI
import java.nio.file.Paths

/**
 * Collapses duplicate JVM dependency variants that Bazel's
 * `transitive_compile_time_jars` often lists together (e.g. full jar +
 * `processed_*.jar`, or unstamped + `-stamped.jar`), keeping a single jar
 * per logical artifact aligned with rules_scala's Scalac classpath.
 */
object BazelClasspathNormalizer {

  def normalizeCompileClasspath(uris: List[String]): List[String] = {
    if (uris.isEmpty) return uris
    val jarEntries = uris.zipWithIndex.flatMap { case (uri, idx) =>
      val name = fileName(uri)
      if (!name.toLowerCase.endsWith(".jar")) None
      else Some((jarDedupeGroupKey(uri, name), idx, uri, name))
    }
    if (jarEntries.isEmpty) return uris.distinct

    val winnerUriByKey = jarEntries
      .groupBy(_._1)
      .view
      .mapValues { group =>
        group.maxBy { case (_, idx, _, name) =>
          (jarPreferenceScore(name), -idx)
        }
      }
      .mapValues(_._3)
      .toMap

    val winnerUris = winnerUriByKey.values.toSet
    uris.filter { u =>
      val n = fileName(u)
      !n.toLowerCase.endsWith(".jar") || winnerUris.contains(u)
    }.distinct
  }

  /**
   * Versioned artifacts (Maven-style names) dedupe globally so e.g. stamped and
   * unstamped scala3-library in different trees collapse to one jar; short
   * names like `lib.jar` are scoped by parent so unrelated jars are not merged.
   */
  private def jarDedupeGroupKey(uriStr: String, jarBasename: String): String = {
    val k = artifactKey(jarBasename)
    if (isVersionedArtifactKey(k)) k
    else s"${parentPath(uriStr)}\u0000$k"
  }

  private def isVersionedArtifactKey(key: String): Boolean =
    key.length > 3 && """(^|[-._])\d""".r.findFirstIn(key).nonEmpty

  private def parentPath(uriStr: String): String =
    try {
      val path = new URI(uriStr).getPath
      if (path == null || path.isEmpty) ""
      else {
        val parent = Paths.get(path).getParent
        if (parent == null) "" else parent.toString
      }
    } catch {
      case _: Exception => ""
    }

  /** Same logical artifact across header/processed/stamped naming. */
  def artifactKey(jarBasename: String): String = {
    var n = jarBasename.stripSuffix(".jar").stripSuffix(".JAR")
    if (n.toLowerCase.endsWith("-stamped"))
      n = n.dropRight("-stamped".length)
    while (n.startsWith("header_")) n = n.stripPrefix("header_")
    while (n.startsWith("processed_")) n = n.stripPrefix("processed_")
    n
  }

  /**
   * Higher score wins. Stamped jars match rules_scala Scalac; header/ijar
   * beat raw full jars when those are the only variants.
   */
  private def jarPreferenceScore(basename: String): Int = {
    val lower = basename.toLowerCase
    if (lower.endsWith("-stamped.jar")) 100
    else if (basename.startsWith("header_")) 85
    else if (lower.endsWith("-ijar.jar") || lower.endsWith("_ijar.jar")) 78
    else if (basename.startsWith("processed_")) 25
    else 50
  }

  private def fileName(uriStr: String): String = {
    try {
      val path = new URI(uriStr).getPath
      if (path == null || path.isEmpty) ""
      else {
        val p = Paths.get(path)
        val name = p.getFileName
        if (name == null) "" else name.toString
      }
    } catch {
      case _: Exception => ""
    }
  }
}
