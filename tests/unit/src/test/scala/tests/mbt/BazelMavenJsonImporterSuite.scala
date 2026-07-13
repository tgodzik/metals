package tests.mbt

import java.nio.file.Files
import java.util.zip.ZipOutputStream

import scala.meta.internal.metals.mbt.importer.BazelMavenJsonImporter
import scala.meta.internal.metals.mbt.importer.BazelMbtImporter
import scala.meta.io.AbsolutePath

class BazelMavenJsonImporterSuite extends tests.BaseSuite {

  private def createEmptyJar(
      dir: java.nio.file.Path,
      name: String,
  ): java.nio.file.Path = {
    val jar = dir.resolve(name)
    val zos = new ZipOutputStream(Files.newOutputStream(jar))
    zos.close()
    jar
  }

  test("no-processor-info-from-maven-import") {
    val dir = Files.createTempDirectory("bazel-maven-importer")
    val lombokJar = createEmptyJar(dir, "lombok-1.18.34.jar")

    val mavenInstallJson =
      s"""|{
          |  "artifacts": {
          |    "org.projectlombok:lombok": {
          |      "version": "1.18.34",
          |      "shasums": {}
          |    }
          |  }
          |}
          |""".stripMargin

    val lockFile = dir.resolve("maven_install.json")
    Files.writeString(lockFile, mavenInstallJson)

    val savedHome = System.getProperty("user.home")
    val m2RepoDir =
      dir.resolve("m2_home/.m2/repository/org/projectlombok/lombok/1.18.34")
    Files.createDirectories(m2RepoDir)
    Files.copy(lombokJar, m2RepoDir.resolve("lombok-1.18.34.jar"))
    System.setProperty(
      "user.home",
      dir.resolve("m2_home").toAbsolutePath.toString,
    )

    try {
      val modules = BazelMavenJsonImporter.importMaven(
        AbsolutePath(dir),
        outputBase = None,
        repositoryName = "maven",
      )

      val lombokModule =
        modules.find(_.id == "org.projectlombok:lombok:1.18.34")
      assert(clue(lombokModule).isDefined)
      assert(lombokModule.get.getAnnotationProcessors.isEmpty)
    } finally {
      System.setProperty("user.home", savedHome)
    }
  }

  test("plugins-from-query-xml") {
    val xml =
      """|<?xml version="1.0" encoding="UTF-8"?>
         |<query version="2">
         |  <rule class="java_plugin" name="//tools:auto_value_plugin" location="tools/BUILD:1">
         |    <string name="processor_class" value="com.google.auto.value.processor.AutoValueProcessor"/>
         |    <list name="deps">
         |      <label value="@maven//:com_google_auto_value_auto_value"/>
         |      <label value="@maven//:com_google_auto_value_extensions_auto_value_gson"/>
         |    </list>
         |  </rule>
         |  <rule class="java_library" name="//some:lib" location="some/BUILD:1">
         |    <list name="deps">
         |      <label value="@maven//:com_google_guava_guava"/>
         |    </list>
         |  </rule>
         |</query>
         |""".stripMargin

    val plugins = BazelMbtImporter.pluginsFromQueryXml(xml)
    assertEquals(plugins.size, 1)
    val (processorClass, depLabels) = plugins.head
    assertEquals(
      processorClass,
      "com.google.auto.value.processor.AutoValueProcessor",
    )
    assertEquals(
      depLabels.sorted,
      Seq(
        "@maven//:com_google_auto_value_auto_value",
        "@maven//:com_google_auto_value_extensions_auto_value_gson",
      ),
    )
  }

  test("no-annotation-processors-for-plain-jar") {
    val dir = Files.createTempDirectory("bazel-maven-importer-plain")
    val guavaJar = createEmptyJar(dir, "guava-32.0.0.jar")

    val mavenInstallJson =
      s"""|{
          |  "artifacts": {
          |    "com.google.guava:guava": {
          |      "version": "32.0.0",
          |      "shasums": {}
          |    }
          |  }
          |}
          |""".stripMargin

    val lockFile = dir.resolve("maven_install.json")
    Files.writeString(lockFile, mavenInstallJson)

    val savedHome = System.getProperty("user.home")
    val m2RepoDir =
      dir.resolve(".m2_home/.m2/repository/com/google/guava/guava/32.0.0")
    Files.createDirectories(m2RepoDir)
    Files.copy(guavaJar, m2RepoDir.resolve("guava-32.0.0.jar"))
    System.setProperty(
      "user.home",
      dir.resolve(".m2_home").toAbsolutePath.toString,
    )

    try {
      val modules = BazelMavenJsonImporter.importMaven(
        AbsolutePath(dir),
        outputBase = None,
        repositoryName = "maven",
      )

      val guavaModule = modules.find(_.id == "com.google.guava:guava:32.0.0")
      assert(clue(guavaModule).isDefined)
      assert(guavaModule.get.getAnnotationProcessors.isEmpty)
      assert(guavaModule.get.annotationProcessors == null)
    } finally {
      System.setProperty("user.home", savedHome)
    }
  }
}
