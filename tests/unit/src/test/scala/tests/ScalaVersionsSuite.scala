package tests

import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.internal.semver.SemVer

class ScalaVersionsSuite extends BaseSuite {

  test("idempotent-minor-release") {
    assert(
      ScalaVersions.dropVendorSuffix("2.12.4") ==
        "2.12.4"
    )
  }

  test("retain-pre-release-version") {
    assert(
      ScalaVersions.dropVendorSuffix("2.13.0-RC1") ==
        "2.13.0-RC1"
    )
    assert(
      ScalaVersions.dropVendorSuffix("2.13.0-M5") ==
        "2.13.0-M5"
    )
  }

  test("drop-typelevel-vendor-suffix") {
    assert(
      ScalaVersions.dropVendorSuffix("2.12.4-bin-typelevel-4") ==
        "2.12.4"
    )
  }

  test("recommended-future") {
    assert(
      ScalaVersions.recommendedVersion(V.scala212 + "1") ==
        V.scala212
    )
  }

  test("recommended-212") {
    assert(
      ScalaVersions.recommendedVersion("2.12.4") ==
        V.scala212
    )
  }

  test("recommended-211") {
    assert(
      ScalaVersions.recommendedVersion("2.11.4") ==
        V.scala212
    )
  }

  test("recommended-213") {
    assert(
      ScalaVersions.recommendedVersion("2.13.0") ==
        V.scala213
    )
  }

  test("future-213") {
    assert(
      ScalaVersions.isFutureVersion("2.13.31")
    )
  }

  test("not-future-213") {
    assert(
      !ScalaVersions.isFutureVersion("2.13.1")
    )
  }

  test("future-212") {
    assert(
      ScalaVersions.isFutureVersion("2.12.31")
    )
  }

  test("not-future-212") {
    assert(
      !ScalaVersions.isFutureVersion("2.12.10")
    )
  }

  test("not-future-211") {
    assert(
      !ScalaVersions.isFutureVersion("2.11.10")
    )
  }

  test("future-214") {
    assert(
      ScalaVersions.isFutureVersion("2.15.10")
    )
  }

  test("future-315") {
    assert(
      ScalaVersions.isFutureVersion("3.15.10")
    )
  }

  test("2.12.11-comapatible-with-2.12.5") {
    assert(
      SemVer.isCompatibleVersion("2.12.5", "2.12.11")
    )
  }

  test("2.12.5-not-compatible-with-2.12.11") {
    assert(
      !SemVer.isCompatibleVersion("2.12.11", "2.12.5")
    )
  }

  test("2.12.7-compatible-with-2.12.5") {
    assert(
      SemVer.isCompatibleVersion("2.12.5", "2.12.7")
    )
  }

  test("2.12.5-not-compatible-with-2.12.7") {
    assert(
      !SemVer.isCompatibleVersion("2.12.7", "2.12.5")
    )
  }

  test("2.12.11-compatible-with-2.11.12") {
    assert(
      SemVer.isCompatibleVersion("2.11.12", "2.12.11")
    )
  }

  test("2.11.12-not-compatible-with-2.12.11") {
    assert(
      !SemVer.isCompatibleVersion("2.12.11", "2.11.12")
    )
  }

  test("recommended-3") {
    assert(
      ScalaVersions.recommendedVersion("0.21.0") ==
        V.scala3
    )
  }

  test("supported-3") {
    assert(
      ScalaVersions.isSupportedScalaVersion(
        "3.0.0-M2-bin-SNAPSHOT-nonbootstrapped"
      ) ==
        true
    )
  }
}
