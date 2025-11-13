package scala.meta.internal.pc

import java.nio.file.Path
import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeVariable
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.element.VariableElement
import javax.lang.model.util.Elements

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearchVisitor

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import org.eclipse.lsp4j
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.SymbolKind
import scala.collection.mutable.ListBuffer

class JavaCompletionProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams,
    isCompletionSnippetsEnabled: Boolean,
    buildTargetIdentifier: String
) {

  lazy val identifier = extractIdentifier.toLowerCase
  def completions(): CompletionList = {
    val nextIsWhitespace =
      if (params.offset() < params.text().length())
        params.text().charAt(params.offset()).isWhitespace
      else false
    val textWithSemicolon =
      if (nextIsWhitespace)
        params.text().substring(0, params.offset()) +
          ";" +
          params.text().substring(params.offset())
      else params.text()
    val task: JavacTask =
      compiler.compilationTask(textWithSemicolon, params.uri())

    val scanner = JavaMetalsGlobal.scanner(task)
    val position =
      CursorPosition(params.offset(), params.offset(), params.offset())
    val node = compiler.compilerTreeNode(scanner, position)

    node match {
      case Some(n) =>
        val trees = Trees.instance(task)
        // TODO make sure proper newlines are always added and that we add at the bottom
        val importPosition = n
          .iterator()
          .asScala
          .dropWhile { tree =>
            tree.getKind() != Kind.PACKAGE
          }
          .toList
          .headOption match {
          case Some(value) =>
            val sourcePositions = trees.getSourcePositions()
            val end = sourcePositions.getEndPosition(scanner.root, value)
            val pos = compiler.offsetToPosition(end.toInt, params.text())
            new lsp4j.Range(pos, pos)
          case None =>
            new lsp4j.Range(new lsp4j.Position(0, 0), new lsp4j.Position(0, 0))
        }
        val items = n.getLeaf.getKind match {
          case Kind.MEMBER_SELECT =>
            completeMemberSelect(task, n, trees, importPosition).distinct
          case Kind.IDENTIFIER =>
            completeFromScope(
              task,
              n,
              trees,
              importPosition
            ).distinct ++ keywords(n)
          case _ => keywords(n)
        }
        new CompletionList(items.asJava)
      case None => new CompletionList()
    }
  }

  private def identifierScore(element: Element): Int = {
    val name = element.getSimpleName().toString().toLowerCase()
    name.indexOf(identifier) match {
      case 0 => 0
      case -1 => 2
      case _ => 1
    }
  }

  private def memberScore(element: Element, containingElement: Element): Int = {
    val idScore = identifierScore(element)
    val memberScore =
      if (element.getEnclosingElement() == containingElement) 0 else 1
    idScore << 1 | memberScore
  }

  private def completeMemberSelect(
      task: JavacTask,
      path: TreePath,
      trees: Trees,
      importPosition: lsp4j.Range
  ): List[CompletionItem] = {
    val typeAnalyzer = new JavaTypeAnalyzer(task)
    val select = path.getLeaf.asInstanceOf[MemberSelectTree]
    val newPath = new TreePath(path, select.getExpression)
    val memberType = typeAnalyzer.typeMirror(newPath)
    val exprElem = trees.getElement(newPath)

    val isStaticContext =
      exprElem != null && (exprElem.getKind match {
        case ElementKind.CLASS | ElementKind.INTERFACE | ElementKind.ENUM |
            ElementKind.ANNOTATION_TYPE | ElementKind.TYPE_PARAMETER =>
          true
        case _ => false
      })

    memberType match {
      case dt: DeclaredType =>
        completeDeclaredType(task, dt, importPosition, isStaticContext)
      case _: ArrayType => completeArrayType()
      case tv: TypeVariable => completeTypeVariable(task, tv, importPosition)
      case _ => Nil
    }
  }

  private def completeFromScope(
      task: JavacTask,
      path: TreePath,
      trees: Trees,
      importPosition: lsp4j.Range
  ): List[CompletionItem] = {
    val scope = trees.getScope(path)

    val scopeCompletion =
      JavaScopeVisitor.scopeMembers(task, scope).map(SimpleElement.apply)

    val identifier = extractIdentifier

    val importableElements = ListBuffer.empty[ImportableElement]

    val visitor = new JavaClassVisitor(
      task.getElements(),
      element => {
        importableElements.addOne(ImportableElement(element))
        true
      }
    )
    compiler.search.search(identifier, buildTargetIdentifier, visitor)
    val all: List[ScopeElement] =
      (scopeCompletion ++ importableElements.toList)
    all
      .distinctBy(_.element)
      .sortBy(el => identifierScore(el.element))
      .map(el => completionItem(el, importPosition))
      .filter(item => CompletionFuzzy.matches(identifier, item.getLabel))
  }

  private def completeDeclaredType(
      task: JavacTask,
      declaredType: DeclaredType,
      importPosition: lsp4j.Range,
      isStaticContext: Boolean = false
  ): List[CompletionItem] = {
    // constructors cannot be invoked as members
    val bannedKinds = Set(
      ElementKind.CONSTRUCTOR,
      ElementKind.STATIC_INIT,
      ElementKind.INSTANCE_INIT
    )
    val declaredElement = declaredType.asElement()
    val members = task.getElements
      .getAllMembers(declaredElement.asInstanceOf[TypeElement])
      .asScala
      .toList
      .map(SimpleElement.apply)

    val identifier = extractIdentifier

    val importableElements = ListBuffer.empty[ImportableElement]
    val visitor = new JavaClassVisitor(
      task.getElements(),
      element => {
        importableElements.addOne(ImportableElement(element))
        true
      }
    )
    compiler.search.search(identifier, buildTargetIdentifier, visitor)
    val all: List[ScopeElement] =
      (members ++ importableElements.toList)
    val completionItems = all
      .filter { member =>
        val isMatches = CompletionFuzzy.matches(
          identifier,
          member.element.getSimpleName.toString
        )
        val isAllowedKind = !bannedKinds(member.element.getKind())
        val isStaticMember =
          member.element.getModifiers.contains(Modifier.STATIC)

        isMatches && isAllowedKind && (isStaticMember || !isStaticContext)
      }
      .sortBy { element =>
        memberScore(element.element, declaredElement)
      }
      .map(el => completionItem(el, importPosition))
    completionItems
  }

  private def extractIdentifier: String = {
    val start = inferIdentStart(params.offset(), params.text())
    val end = params.offset()

    params.text().substring(start, end)
  }

  private def keywords(path: TreePath): List[CompletionItem] = {
    val identifier = extractIdentifier
    val level = keywordLevel(path)

    JavaKeyword.all
      .collect {
        case keyword
            if keyword.level == level && CompletionFuzzy.matches(
              identifier,
              keyword.name
            ) =>
          keyword.name
      }
      .map { keyword =>
        val item = new CompletionItem(keyword)
        item.setKind(CompletionItemKind.Keyword)
        item
      }

  }

  @tailrec
  private def keywordLevel(path: TreePath): JavaKeyword.Level = {
    if (path == null) JavaKeyword.TopLevel
    else {
      path.getLeaf match {
        case _: MethodTree => JavaKeyword.MethodLevel
        case _: ClassTree => JavaKeyword.ClassLevel
        case _: CompilationUnitTree => JavaKeyword.TopLevel
        case _ => keywordLevel(path.getParentPath)
      }
    }
  }

  private def completeArrayType(): List[CompletionItem] = {
    val identifier = extractIdentifier
    if (CompletionFuzzy.matches(identifier, "length")) {
      val item = new CompletionItem("length")
      item.setKind(CompletionItemKind.Keyword)
      List(item)
    } else {
      Nil
    }

  }

  @tailrec
  private def completeTypeVariable(
      task: JavacTask,
      typeVariable: TypeVariable,
      importPosition: lsp4j.Range
  ): List[CompletionItem] = {
    typeVariable.getUpperBound match {
      case dt: DeclaredType => completeDeclaredType(task, dt, importPosition)
      case tv: TypeVariable => completeTypeVariable(task, tv, importPosition)
      case _ => Nil
    }
  }

  private def inferIdentStart(pos: Int, text: String): Int = {
    var i = pos - 1
    while (i >= 0 && Character.isJavaIdentifierPart(text.charAt(i))) {
      i -= 1
    }
    i + 1
  }

  private def completionItem(
      scopeElement: ScopeElement,
      importPosition: lsp4j.Range
  ): CompletionItem = {
    val element = scopeElement.element
    val simpleName = element.getSimpleName.toString

    val (label, insertText) = element match {
      case e: ExecutableElement
          if isCompletionSnippetsEnabled && e.getParameters.size() > 0 =>
        (JavaLabels.executableLabel(e), s"$simpleName($$0)")
      case e: ExecutableElement =>
        (JavaLabels.executableLabel(e), s"$simpleName()")
      case _ => (simpleName, simpleName)
    }

    val importStatement = scopeElement match {
      case SimpleElement(element) => Nil
      case ImportableElement(element) =>
        val toImport = element.getEnclosingElement.toString()
        List(
          new lsp4j.TextEdit(
            importPosition,
            s"\nimport $toImport.${scopeElement.element.getSimpleName()};"
          )
        )

    }

    val item = new CompletionItem(label)

    if (isCompletionSnippetsEnabled)
      item.setInsertTextFormat(InsertTextFormat.Snippet)

    item.setInsertText(insertText)
    item.setAdditionalTextEdits(importStatement.asJava)

    val kind = completionKind(element.getKind)
    kind.foreach(item.setKind)

    val detail = element match {
      case v: VariableElement => JavaLabels.typeLabel(v.asType())
      case e: ExecutableElement => JavaLabels.typeLabel(e.asType())
      case t: TypeElement => JavaLabels.typeLabel(t.asType())
      case _ => ""
    }

    item.setDetail(detail)

    item
  }

  private def completionKind(k: ElementKind): Option[CompletionItemKind] =
    k match {
      case ElementKind.CLASS => Some(CompletionItemKind.Class)
      case ElementKind.ENUM => Some(CompletionItemKind.Enum)
      case ElementKind.ANNOTATION_TYPE => Some(CompletionItemKind.Interface)
      case ElementKind.INTERFACE => Some(CompletionItemKind.Interface)
      case ElementKind.CONSTRUCTOR => Some(CompletionItemKind.Constructor)
      case ElementKind.TYPE_PARAMETER => Some(CompletionItemKind.TypeParameter)
      case ElementKind.FIELD => Some(CompletionItemKind.Field)
      case ElementKind.PACKAGE => Some(CompletionItemKind.Module)
      case ElementKind.LOCAL_VARIABLE => Some(CompletionItemKind.Variable)
      case ElementKind.RESOURCE_VARIABLE => Some(CompletionItemKind.Variable)
      case ElementKind.PARAMETER => Some(CompletionItemKind.Property)
      case ElementKind.METHOD => Some(CompletionItemKind.Method)
      case _ => None
    }

}

sealed trait ScopeElement {
  def element: Element
}
case class SimpleElement(element: Element) extends ScopeElement

case class ImportableElement(element: Element) extends ScopeElement

class JavaClassVisitor(elements: Elements, visitMember: Element => Boolean)
    extends SymbolSearchVisitor {
  private def toDotPackage(pkg: String) =
    pkg.replace("/", ".").stripSuffix(".")
  override def shouldVisitPackage(pkg: String): Boolean = {
    !elements.getAllPackageElements(toDotPackage(pkg)).isEmpty()
  }

  override def visitClassfile(pkg: String, filename: String): Int = {
    val all =
      elements.getAllPackageElements(toDotPackage(pkg)).asScala.flatMap { pkg =>
        pkg.getEnclosedElements().asScala.find { cls =>
          cls.getSimpleName().toString + ".class" == filename
        }

      }
    all.filter(visitMember).size
  }

  override def visitWorkspaceSymbol(
      path: Path,
      symbol: String,
      kind: SymbolKind,
      range: lsp4j.Range
  ): Int = {
    pprint.log(path)
    0
  }

  override def isCancelled(): Boolean = false

}
