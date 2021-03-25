package com.scalatsi

import java.util.regex.Pattern

import scala.collection.immutable.ListMap
import TypescriptType._

sealed trait TypescriptType {
  def |(tt: TypescriptType): TSUnion = this match {
    case TSUnion(of) => TSUnion(of :+ tt)
    case _           => TSUnion.of(this, tt)
  }

  def array: TSArray = TSArray(this)
}

object TypescriptType {
  private[scalatsi] def fromString(tpe: String): TypescriptType =
    tpe match {
      case "any"       => TSAny
      case "boolean"   => TSBoolean
      case "never"     => TSNever
      case "null"      => TSNull
      case "number"    => TSNumber
      case "string"    => TSString
      case "undefined" => TSUndefined
      case "void"      => TSVoid
      case "object"    => TSObject
      case _           => TSTypeReference(tpe)
    }

  /** Get a reference to a named type, or the type itself if it is unnamed or built-in */
  def nameOrType(tpe: TypescriptType, discriminator: Option[String] = None): TypescriptType = tpe match {
    case named: TypescriptNamedType => named.asReference(discriminator)
    case anonymous                  => anonymous
  }

  /** A marker trait for a TS type that has a name */
  sealed trait TypescriptNamedType extends TypescriptType {
    def name: String
    require(isValidTSName(name), s"Not a valid TypeScript identifier: $name")

    def asReference(discriminator: Option[String] = None): TSTypeReference = TSTypeReference(name, Some(this), discriminator)

    def withName(newName: String): TypescriptNamedType
  }
  object TypescriptNamedType {
    implicit val ordering: Ordering[TypescriptNamedType] = Ordering.by[TypescriptNamedType, String](_.name)
  }

  /** A marker trait for a TS type that can contain nested types */
  sealed trait TypescriptAggregateType extends TypescriptType {
    def nested: Set[TypescriptType]
  }
  object TypescriptAggregateType {
    def unapply(aggregateType: TypescriptAggregateType): Option[Set[TypescriptType]] =
      Some(aggregateType.nested)
  }

  /** `type name = underlying` */
  case class TSAlias(name: String, underlying: TypescriptType) extends TypescriptNamedType with TypescriptAggregateType {
    override def nested: Set[TypescriptType]        = Set(underlying)
    override def withName(newName: String): TSAlias = copy(name = newName)
  }

  case class TSTaggedPrimitive(tag: String, underlying: TypescriptType) extends TypescriptType

  case object TSAny                               extends TypescriptType
  case class TSArray(elementType: TypescriptType) extends TypescriptAggregateType { def nested: Set[TypescriptType] = Set(elementType) }
  case object TSBoolean                           extends TypescriptType

  sealed trait TSLiteralType[T]                 extends TypescriptType { val value: T }
  case class TSLiteralString(value: String)     extends TSLiteralType[String]
  case class TSLiteralNumber(value: BigDecimal) extends TSLiteralType[BigDecimal]
  case class TSLiteralBoolean(value: Boolean)   extends TSLiteralType[Boolean]

  case class TSEnum(name: String, const: Boolean, entries: ListMap[String, Option[Int]])
    extends TypescriptNamedType
    with TypescriptAggregateType {
    def nested: Set[TypescriptType]                = Set(TSNumber)
    override def withName(newName: String): TSEnum = copy(name = newName)
  }

  /** This type is used as a marker that a type with this name exists and is either already defined or externally defined.
    * Not a real Typescript type
    * @note name takes from [Typescript specification](https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#3.8.2)
    * @param impl The implementation of the type if it is known, so that the nested types can be outputted even if not directly referenced
    * @param discriminator the discrimininator value for the type if this type is part of a discriminated union
    */
  case class TSTypeReference(name: String, impl: Option[TypescriptType] = None, discriminator: Option[String] = None)
    extends TypescriptNamedType
    with TypescriptAggregateType {
    override def asReference(discriminator: Option[String] = None): TSTypeReference = this
    override def nested: Set[TypescriptType]                                        = impl.toSet
    override def withName(newName: String): TSTypeReference                         = copy(name = newName)
  }
  @deprecated("0.2.0", "Renamed to TSTypeReference")
  type TSExternalName = TSTypeReference

  /** Typescript anonymous indexed interfaces
    * { [indexName:indexType]: valueType }
    * @param indexType index type, TSNumber or TSString
    */
  case class TSIndexedInterface(indexName: String = "key", indexType: TypescriptType, valueType: TypescriptType)
    extends TypescriptAggregateType {
    require(
      indexType == TSString || indexType == TSNumber,
      s"TypeScript indexed interface can only have index type string or number, not $indexType"
    )
    def nested: Set[TypescriptType] = Set(indexType, valueType)
  }
  case class TSInterfaceIndexed(name: String, indexName: String = "key", indexType: TypescriptType, valueType: TypescriptType)
    extends TypescriptNamedType
    with TypescriptAggregateType {
    require(
      indexType == TSString || indexType == TSNumber,
      s"TypeScript indexed interface $name can only have index type string or number, not $indexType"
    )
    def nested: Set[TypescriptType]                            = Set(indexType, valueType)
    override def withName(newName: String): TSInterfaceIndexed = copy(name = newName)
  }

  case class TSInterface(name: String, members: ListMap[String, TypescriptType]) extends TypescriptNamedType with TypescriptAggregateType {
    def nested: Set[TypescriptType] = members.values.toSet

    override def withName(newName: String): TSInterface = copy(name = newName)
  }

  case class TSIntersection(of: Seq[TypescriptType]) extends TypescriptAggregateType { def nested: Set[TypescriptType] = of.toSet }
  object TSIntersection {
    def of(of: TypescriptType*): TSIntersection = TSIntersection(of)
  }
  case object TSNever  extends TypescriptType
  case object TSNull   extends TypescriptType
  case object TSNumber extends TypescriptType
  case object TSObject extends TypescriptType
  case object TSString extends TypescriptType

  /** Typescript tuple: `[0.type, 1.type, ... n.type]` */
  // TODO: Why does this have a generic parameters? Remove?
  case class TSTuple[E](of: Seq[TypescriptType]) extends TypescriptAggregateType { def nested: Set[TypescriptType] = of.toSet }
  object TSTuple {
    def of(of: TypescriptType*): TSTuple[Any] = TSTuple(of)
  }

  case object TSUndefined extends TypescriptType
  case class TSUnion(of: Seq[TypescriptType]) extends TypescriptAggregateType {
    def nested: Set[TypescriptType] = of.toSet
  }
  object TSUnion {
    def of(of: TypescriptType*): TSUnion = TSUnion(of)
  }
  case object TSVoid extends TypescriptType

  private val tsIdentifierPattern = Pattern.compile("[_$\\p{L}\\p{Nl}][_$\\p{L}\\p{Nl}\\p{Nd}\\{Mn}\\{Mc}\\{Pc}]*")
  private[scalatsi] def isValidTSName(name: String): Boolean =
    tsIdentifierPattern.matcher(name).matches() && !reservedKeywords.contains(name)

  final private[scalatsi] val reservedKeywords: Set[String] = Set(
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
}
