package scala.meta.internal.metals

import scala.meta.internal.mtags
import scala.meta.internal.semver.SemVer

object ScalaVersions {

  def isScala3Milestone(version: String) =
    version.startsWith("3.0.0-M") || version.startsWith("3.0.0-RC")

  /**
   * Non-Lightbend compilers often use a suffix, such as `-bin-typelevel-4`
   */
  def dropVendorSuffix(version: String): String = {
    if (isScala3Version(version) && version.contains("NIGHTLY"))
      version
    else if (isScala3Version(version) && version.contains("SNAPSHOT"))
      version.replace("-nonbootstrapped", "")
    else
      version.replaceAll("-bin-.*", "")
  }

  private val _isDeprecatedScalaVersion: Set[String] =
    BuildInfo.deprecatedScalaVersions.toSet
  private val _isSupportedScalaVersion: Set[String] =
    BuildInfo.supportedScalaVersions.toSet

  def isSupportedScalaVersion(version: String): Boolean = {
    _isSupportedScalaVersion(dropVendorSuffix(version))
  }

  def isDeprecatedScalaVersion(version: String): Boolean =
    _isDeprecatedScalaVersion(dropVendorSuffix(version))

  def isSupportedScalaBinaryVersion(scalaVersion: String): Boolean =
    BuildInfo.supportedScalaBinaryVersions.exists { binaryVersion =>
      scalaVersion.startsWith(binaryVersion)
    }

  def isScala3Version(scalaVersion: String): Boolean =
    scalaVersion.startsWith("0.") || scalaVersion.startsWith("3.")

  def supportedScala3Versions: Set[String] =
    BuildInfo.supportedScalaVersions.filter(isScala3Version(_)).toSet

  val isLatestScalaVersion: Set[String] =
    Set(BuildInfo.scala212, BuildInfo.scala213, BuildInfo.scala3)

  def latestBinaryVersionFor(scalaVersion: String): Option[String] = {
    val binaryVersion = scalaBinaryVersionFromFullVersion(scalaVersion)
    isLatestScalaVersion
      .find(latest =>
        binaryVersion == scalaBinaryVersionFromFullVersion(latest)
      )
  }

  def recommendedVersion(scalaVersion: String): String = {
    latestBinaryVersionFor(scalaVersion).getOrElse {
      if (isScala3Version(scalaVersion)) {
        BuildInfo.scala3
      } else {
        BuildInfo.scala212
      }
    }
  }

  def isFutureVersion(scalaVersion: String): Boolean = {
    latestBinaryVersionFor(scalaVersion)
      .map(latest =>
        latest != scalaVersion && SemVer
          .isCompatibleVersion(latest, scalaVersion)
      )
      .getOrElse {
        val versions =
          if (isScala3Version(scalaVersion))
            isLatestScalaVersion.filter(isScala3Version)
          else
            isLatestScalaVersion.filter(!isScala3Version(_))
        versions.forall(ver => SemVer.isCompatibleVersion(ver, scalaVersion))
      }
  }

  def isCurrentScalaCompilerVersion(version: String): Boolean =
    ScalaVersions.dropVendorSuffix(
      version
    ) == mtags.BuildInfo.scalaCompilerVersion

  def scalaBinaryVersionFromFullVersion(scalaVersion: String): String = {
    if (isScala3Milestone(scalaVersion))
      scalaVersion
    else
      scalaVersion.split('.').take(2).mkString(".")
  }
}
