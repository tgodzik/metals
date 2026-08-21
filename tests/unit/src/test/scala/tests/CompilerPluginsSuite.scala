package tests

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

import scala.meta.internal.metals.CompilerPlugins

class CompilerPluginsSuite extends BaseSuite {

  private val plugins = new CompilerPlugins()

  test("unresolved-csr-cache-missing-jar") {
    val option =
      "-Xplugin:${CSR_CACHE}/https/repo1.maven.org/maven2/org/scalameta/semanticdb-scalac_2.13.18/4.17.0/semanticdb-scalac_2.13.18-4.17.0.jar"
    val result = plugins.filterSupportedOptions(
      Seq("-Yrangepos", option, "-P:semanticdb:synthetics:on")
    )
    assertEquals(result, Seq("-Yrangepos"))
  }

  test("keep-supported-plugin-jar") {
    val jar = pluginJar("bm4")
    val option = s"-Xplugin:$jar"
    assertEquals(
      plugins.filterSupportedOptions(Seq("-Yrangepos", option)),
      Seq("-Yrangepos", option),
    )
  }

  test("filter-unsupported-plugin-jar") {
    val jar = pluginJar("semanticdb")
    val result = plugins.filterSupportedOptions(
      Seq(s"-Xplugin:$jar", "-P:semanticdb:synthetics:on")
    )
    assertEquals(result, Seq.empty[String])
  }

  private def pluginJar(name: String): Path = {
    val jar =
      Files.createTempDirectory("compiler-plugins").resolve(s"$name.jar")
    val zos = new ZipOutputStream(new FileOutputStream(jar.toFile))
    try {
      zos.putNextEntry(new ZipEntry("scalac-plugin.xml"))
      val xml = s"<plugin><name>$name</name></plugin>"
      zos.write(xml.getBytes(StandardCharsets.UTF_8))
      zos.closeEntry()
    } finally {
      zos.close()
    }
    jar
  }
}
