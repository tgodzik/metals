package tests

class QueryLspSuite extends BaseLspSuite("query") {

  test("findPackage") {
    for {
      _ <- initialize(
        """
          |/metals.json
          |{"a":
          |  { 
          |    "scalaVersion": "3.3.5",
          |    "libraryDependencies": [
          |       "org.virtuslab::besom-core:0.3.2",
          |       "org.virtuslab::besom-aws:6.31.0-core.0.3"
          |    ]
          |  } 
          |}
          |/a/src/main/scala/a/A.scala
          |package a
          |object A
          |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/a/A.scala")
      _ = assertNoDiagnostics()
      locations = server.server.queryEngine.findPackage("docdb").mkString("\n")
      _ = assertEquals(
        locations,
        """|besom/api/aws/docdb/inputs/
           |besom/api/aws/docdb/
           |besom/api/aws/docdb/outputs/
           |""".stripMargin,
      )
    } yield ()
  }
}
