package tests.bazelnative

import scala.meta.internal.builds.bazelnative.BazelClasspathNormalizer

import munit.FunSuite

class BazelClasspathNormalizerSuite extends FunSuite {

  private def u(name: String): String =
    java.nio.file.Paths.get("/execroot", "external", "x", name).toUri.toString

  test("artifactKey strips header processed stamped") {
    assertEquals(
      BazelClasspathNormalizer.artifactKey("header_scala3-library_3-3.3.7.jar"),
      "scala3-library_3-3.3.7",
    )
    assertEquals(
      BazelClasspathNormalizer.artifactKey(
        "processed_scala3-library_3-3.3.7.jar"
      ),
      "scala3-library_3-3.3.7",
    )
    assertEquals(
      BazelClasspathNormalizer.artifactKey(
        "scala3-library_3-3.3.7-stamped.jar"
      ),
      "scala3-library_3-3.3.7",
    )
  }

  test("drops processed_ when plain jar exists for same artifact") {
    val plain = u("scala3-library_3-3.3.7.jar")
    val proc = u("processed_scala3-library_3-3.3.7.jar")
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(plain, proc))
    assertEquals(out, List(plain))
  }

  test("prefers -stamped.jar over unstamped") {
    val stamped = u("scala3-library_3-3.3.7-stamped.jar")
    val plain = u("scala3-library_3-3.3.7.jar")
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(plain, stamped))
    assertEquals(out, List(stamped))
  }

  test("stamped in different directory still wins over plain") {
    val stamped =
      java.nio.file.Paths
        .get(
          "/execroot",
          "rules_scala.stamp",
          "scala3-library_3-3.3.7-stamped.jar",
        )
        .toUri
        .toString
    val plain =
      java.nio.file.Paths
        .get("/execroot", "maven", "scala3-library_3-3.3.7.jar")
        .toUri
        .toString
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(plain, stamped))
    assertEquals(out, List(stamped))
  }

  test("prefers header_ over plain when no stamped") {
    val header = u("header_scala3-library_3-3.3.7.jar")
    val plain = u("scala3-library_3-3.3.7.jar")
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(plain, header))
    assertEquals(out, List(header))
  }

  test("keeps distinct artifacts and order") {
    val a = u("a.jar")
    val b1 = u("b.jar")
    val b2 = u("processed_b.jar")
    val c = u("c.jar")
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(a, b1, b2, c))
    assertEquals(out, List(a, b1, c))
  }

  test("keeps ijar separate from full jar basename") {
    val ijar = u("service2-ijar.jar")
    val classes = u("service2.jar")
    val out =
      BazelClasspathNormalizer.normalizeCompileClasspath(List(ijar, classes))
    assertEquals(out, List(ijar, classes))
  }

  test("same basename in different directories is not merged") {
    val a = java.nio.file.Paths.get("/execroot", "a", "lib.jar").toUri.toString
    val b = java.nio.file.Paths.get("/execroot", "b", "lib.jar").toUri.toString
    val out = BazelClasspathNormalizer.normalizeCompileClasspath(List(a, b))
    assertEquals(out, List(a, b))
  }
}
