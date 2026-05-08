package scala.meta.internal.metals.mbt.importer

import scala.xml.XML
class BazelTargetsXmlDump(xmlDump: String) {

  private lazy val root = XML.loadString(xmlDump)

  def getLabels(attributeName: String): Map[String, List[String]] = {
    val targetLabels = for {
      rule <- root \\ "rule"
      target = (rule \ "@name").text
      if target.nonEmpty
    } yield target -> labelsFromRuleAttribute(rule, Some(attributeName)).toList
    targetLabels.toMap
  }

  def getStrings(attributeName: String): Map[String, List[String]] = {
    val targetLabels = for {
      rule <- root \\ "rule"
      target = (rule \ "@name").text
      if target.nonEmpty
    } yield target -> stringsFromRuleAttribute(rule, attributeName).toList
    targetLabels.toMap
  }

  lazy val depsByTarget: Map[String, List[String]] = {
    val targetLabels = for {
      rule <- root \\ "rule"
      target = (rule \ "@name").text
      if target.nonEmpty
    } yield {
      val ruleInputs = for {
        input <- rule \ "rule-input"
        value = (input \ "@name").text
        if value.nonEmpty
      } yield value
      val labels = (ruleInputs ++ labelsFromRuleAttribute(rule, None)).distinct
      target -> labels.toList
    }
    targetLabels.toMap
  }

  def externalDepsByTarget(targets: List[String]): Map[String, List[String]] = {
    reachableLabels(targets).map { case (target, deps) =>
      target -> deps.filter(isExternalDep)
    }
  }

  private def isExternalDep(label: String): Boolean =
    label.startsWith("@") && !label.startsWith("@@")

  private def reachableLabels(
      rootLabels: List[String]
  ): Map[String, List[String]] =
    rootLabels.map { root =>
      root -> reachableLabels(root, depsByTarget).filterNot(_ == root)
    }.toMap

  private def reachableLabels(
      root: String,
      adjacency: Map[String, List[String]],
  ): List[String] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]
    val queue = scala.collection.mutable.Queue(root)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (!seen(current)) {
        seen += current
        for (dep <- adjacency.getOrElse(current, Nil)) {
          if (!seen(dep)) queue.enqueue(dep)
        }
      }
    }
    seen.toList
  }

  private def labelsFromRuleAttribute(
      rule: scala.xml.Node,
      attributeName: Option[String],
  ): Seq[String] =
    for {
      attribute <- rule \ "_"
      name = (attribute \ "@name").text
      if attributeName.forall(_ == name)
      label <- attribute \\ "label"
      value = (label \ "@value").text
      if value.nonEmpty
    } yield value

  private def stringsFromRuleAttribute(
      rule: scala.xml.Node,
      attributeName: String,
  ): Seq[String] =
    for {
      attribute <- rule \ "_"
      name = (attribute \ "@name").text
      if name == attributeName
      string <- attribute \\ "string"
      value = (string \ "@value").text
      if value.nonEmpty
    } yield value

}
