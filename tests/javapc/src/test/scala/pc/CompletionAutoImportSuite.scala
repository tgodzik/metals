package pc

import tests.pc.BaseJavaCompletionSuite

class CompletionAutoImportSuite extends BaseJavaCompletionSuite {

  check(
    "list",
    """
      |
      |interface A {}
      |
      |class B implements A {
      |
      |    public static int foo() {
      |         Li@@
      |    }
      |
      |}
      |""".stripMargin,
    """
      |LinkageError
      |List
      |List
      |Line
      |Line2D
      |Linker
      |ListUI
      |LinkRef
      |ListPeer
      |LineView
      |ListView
      |UnsatisfiedLinkError
      |""".stripMargin,
  )

  checkEdit(
    "list-edit",
    """
      |
      |interface A {}
      |
      |class B implements A {
      |
      |    public static int foo() {
      |         Lis@@
      |    }
      |
      |}
      |""".stripMargin,
    """
      |import java.util.List;
      |
      |interface A {}
      |
      |class B implements A {
      |
      |    public static int foo() {
      |         Lis
      |    }
      |
      |}
      |""".stripMargin,
    itemIndex = 0,
    assertSingleItem = false,
  )

}
