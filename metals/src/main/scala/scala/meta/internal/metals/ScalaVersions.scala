package scala.meta.internal.metals

import scala.meta.internal.mtags

object ScalaVersions {

  /** Non-Lightbend compilers often use a suffix, such as `-bin-typelevel-4` */
  def dropVendorSuffix(version: String): String =
    version.replaceAll("-bin-.*", "")

  private val _supportedBinaryVersions: Set[String] =
    (BuildInfo.supportedScalaVersions ++ BuildInfo.deprecatedScalaVersions)
      .map(toBinaryVersion)
      .toSet

  private val _isDeprecatedScalaVersion: Set[String] =
    BuildInfo.deprecatedScalaVersions.toSet
  private val _isSupportedScalaVersion: Set[String] =
    BuildInfo.supportedScalaVersions.toSet

  def isSupportedScalaVersion(version: String): Boolean =
    _isSupportedScalaVersion(dropVendorSuffix(version))

  def isDeprecatedScalaVersion(version: String): Boolean =
    _isDeprecatedScalaVersion(dropVendorSuffix(version))

  def isSupportedScalaBinaryVersion(scalaVersion: String): Boolean =
    _supportedBinaryVersions
      .exists { binaryVersion =>
        scalaVersion.startsWith(binaryVersion)
      }

  def toBinaryVersion(scalaVersion: String): String = {
    scalaVersion.split('.').take(2).mkString(".")
  }

  def isScala3Version(scalaVersion: String): Boolean =
    scalaVersion.startsWith("0.")

  val isLatestScalaVersion: Set[String] =
    Set(
      BuildInfo.scala213,
      BuildInfo.scala212,
      BuildInfo.scala211,
      BuildInfo.scala3
    )

  def recommendedVersion(scalaVersion: String): String = {
    if (isScala3Version(scalaVersion)) {
      BuildInfo.scala3
    } else {
      BuildInfo.scala212
    }
  }

  def isCurrentScalaCompilerVersion(version: String): Boolean =
    ScalaVersions.dropVendorSuffix(version) == mtags.BuildInfo.scalaCompilerVersion
}
