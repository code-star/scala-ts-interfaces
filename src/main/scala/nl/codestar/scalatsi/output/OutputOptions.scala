package nl.codestar.scalatsi.output

import java.io.File

import nl.codestar.scalatsi.{TSIdentifier, TSNamespace}

case class OutputOptions(
  targetLocation: File,
  namespaceMapping: Map[String, String],
  ignoredPrefix: String
) {
  lazy val parsedMapping: Map[TSNamespace, TSNamespace] =
    namespaceMapping.map({ case (k, v) => TSNamespace(k) -> TSNamespace(v) })

  lazy val parsedIgnoredPrefix: TSNamespace = TSNamespace(ignoredPrefix)

  private def isValid(ns: TSNamespace) = !ns.parts.contains(TSIdentifier.INVALID)

  def mappingIsValid: Boolean = parsedMapping.forall({ case (k,v) => isValid(k) && isValid(v) })
  def ignoredPrefixIsValid: Boolean = isValid(parsedIgnoredPrefix)
}
