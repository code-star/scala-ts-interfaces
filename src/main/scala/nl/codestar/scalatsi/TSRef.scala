package nl.codestar.scalatsi

import java.util.regex.Pattern

import scala.reflect.runtime.universe.WeakTypeTag
import scala.util.Try
import scala.collection.compat.immutable.ArraySeq

/** This type is used as a marker that a type with this name exists and is either already defined or externally defined
  * @note name takes from [Typescript specification](https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#3.8.2)
  * */
case class TSRef private (name: TSIdentifier, namespace: TSNamespace) {
  override def toString: String = {
    val nss = namespace.toString
    s"$nss${if (nss != "") "." else ""}${name.id}"
  }

  def withName(name: String): TSRef           = this.copy(name = TSIdentifier(name))
  def withNamespace(namespace: String): TSRef = this.copy(namespace = TSNamespace(namespace))
}
object TSRef {
  def apply(name: TSIdentifier, namespace: TSNamespace): TSRef = new TSRef(name, namespace)
  def apply(fqn: String): TSRef = {
    val namespaceWithId = TSNamespace(fqn)
    TSRef(namespaceWithId.parts.last, TSNamespace(namespaceWithId.parts.dropRight(1)))
  }
  def apply[T: WeakTypeTag]: TSRef = TSRef(TSIdentifier[T], TSNamespace[T])

  def unapply(arg: TSRef): Option[(String, Seq[String])] = Some((arg.name.id, arg.namespace.parts.map(_.id)))
}

/** A valid typescript identifier
  * @see https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#382-type-references */
case class TSIdentifier(id: String) {
  import TSIdentifier._
  require(isValidTSName(id))
  override def toString: String = id

  def isValid: Boolean = !(this.eq(TSIdentifier.INVALID) || this.eq(TSIdentifier.UNKNOWN))
}
object TSIdentifier {

  /** Transform a string to a typescript identifier
    * @return [[TSIdentifier.INVALID]] if the identifier is not valid */
  def apply(name: String): TSIdentifier = Try(new TSIdentifier(name)).getOrElse(INVALID)

  /** Transform a type into a typescript identifier */
  def apply[T](implicit tt: WeakTypeTag[T]): TSIdentifier = TSIdentifier(tt.tpe.typeSymbol.name.toString)

  def unapply(arg: TSIdentifier): Option[String] = Some(arg.id)

  def isValidTSName(name: String): Boolean =
    tsIdentifierPattern.matcher(name).matches() && !reservedKeywords.contains(name)

  private val tsIdentifierPattern = Pattern.compile("[_$\\p{L}\\p{Nl}][_$\\p{L}\\p{Nl}\\p{Nd}\\{Mn}\\{Mc}\\{Pc}]*")

  private val reservedKeywords: Set[String] = Set(
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    // Strict mode
    "as",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield"
  )

  // Here because it must be initailized after the above vals
  /** Default identifier to use as a stand-in on the rare ocasion that a JVM name is not a valid Typescript name */
  final val INVALID = TSIdentifier("INVALID")

  /** Identifier to use as a stand-in when the package or name of a JVM type is unknown (e.g. it is anonymous) */
  final val UNKNOWN = TSIdentifier("UNKNOWN")
}

/** A typescript namespace
  * @see https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#3.8.2 */
case class TSNamespace(parts: IndexedSeq[TSIdentifier]) {

  override def toString: String = parts.mkString(".")

  def append(part: TSIdentifier): TSNamespace = new TSNamespace(parts :+ part)

  /** The namespace without a common prefix
    * @example "foo.bar.baz.A" and "foo.bar." will give "baz" */
  def withoutCommonPrefix(other: TSNamespace): TSNamespace = TSNamespace(parts.dropCommonPrefix(other.parts))

  def isValid: Boolean = parts.forall(_.isValid)
}

object TSNamespace {

  /** Identifier to use as a stand-in when the package or name of a JVM type is unknown (e.g. it is anonymous) */
  final val UNKNOWN = TSNamespace(ArraySeq(TSIdentifier.UNKNOWN))

  def apply(parts: String*): TSNamespace    = new TSNamespace(parts.map(TSIdentifier(_)).toIndexedSeq)
  def apply(namespace: String): TSNamespace = TSNamespace(split(namespace): _*)
  def apply[T](implicit tt: WeakTypeTag[T]): TSNamespace =
    Option(split(tt.tpe.typeSymbol.fullName).dropRight(1))
    // If the namespace is not known (e.g. because it is defined in an inner class) it will be "<none>"
      .filterNot(ns => ns.length == 1 && ns.head == "<none>")
      .map(apply(_: _*))
      .getOrElse(UNKNOWN)

  private def split(s: String): IndexedSeq[String] = ArraySeq.unsafeWrapArray(s.split('.'))

  def parse(namespace: String): Try[TSNamespace] = Try(apply(namespace))

  def unapplySeq(arg: TSNamespace): Option[Seq[String]] = Some(arg.parts.map(_.id))
}
