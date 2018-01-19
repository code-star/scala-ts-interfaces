package nl.codestar.scala.ts.interface

import java.util.regex.Pattern

import scala.collection.immutable.ListMap

sealed trait TypescriptType

object TypescriptType {
  private[interface] def fromString(tpe: String): TypescriptType =
    tpe match {
      case "any"       => TSAny
      case "boolean"   => TSBoolean
      case "never"     => TSNever
      case "null"      => TSNull
      case "number"    => TSNumber
      case "string"    => TSString
      case "undefined" => TSUndefined
      case "void"      => TSVoid
      case _           => TSExternalName(tpe)
    }

  /** A marker trait for a TS type that has a name */
  sealed trait TypescriptNamedType extends TypescriptType {
    def name: String
    require(isValidTSName(name), s"Not a valid TypeScript identifier: $name")
  }
  object TypescriptNamedType

  /** A marker trait for a TS type that can contain nested types */
  sealed trait TypescriptAggregateType extends TypescriptType {
    def nested: Set[TypescriptType]
  }
  object TypescriptAggregateType {
    def unapply(
        aggregateType: TypescriptAggregateType): Option[Set[TypescriptType]] =
      Some(aggregateType.nested)
  }

  case class TSAlias(name: String, underlying: TypescriptType)
      extends TypescriptNamedType
  case object TSAny extends TypescriptType
  case class TSArray(elementType: TypescriptType)
      extends TypescriptAggregateType { def nested = Set(elementType) }
  case object TSBoolean extends TypescriptType
  case class TSEnum(name: String,
                    const: Boolean,
                    entries: ListMap[String, Option[Int]])
      extends TypescriptNamedType
      with TypescriptAggregateType {
    def nested = Set(TSNumber)
  }
  // Not really a typescript type, but a marker for us that it is a type with some name that is not known/defined by us
  case class TSExternalName(name: String) extends TypescriptNamedType

  /** Represents Typescript indexed interfaces
    * { [indexName:indexType]: valueType}
    * @param indexType index type, TSNumber or TSString
    **/
  case class TSIndexedInterface(indexName: String = "key",
                                indexType: TypescriptType,
                                valueType: TypescriptType)
      extends TypescriptAggregateType {
    require(
      indexType == TSString || indexType == TSNumber,
      s"TypeScript indexed interface can only have index type string or number, not $indexType")
    def nested = Set(indexType, valueType)
  }
  case class TSInterfaceIndexed(name: String,
                                indexName: String = "key",
                                indexType: TypescriptType,
                                valueType: TypescriptType)
      extends TypescriptNamedType
      with TypescriptAggregateType {
    require(
      indexType == TSString || indexType == TSNumber,
      s"TypeScript indexed interface $name can only have index type string or number, not $indexType")
    def nested = Set(indexType, valueType)
  }
  case class TSInterface(name: String, members: ListMap[String, TypescriptType])
      extends TypescriptNamedType
      with TypescriptAggregateType {
    def nested = members.values.toSet
  }
  case class TSIntersection(of: Seq[TypescriptType])
      extends TypescriptAggregateType { def nested = of.toSet }
  object TSIntersection {
    def of(of: TypescriptType*) = TSIntersection(of)
  }
  case object TSNever extends TypescriptType
  case object TSNull extends TypescriptType
  case object TSNumber extends TypescriptType
  case object TSString extends TypescriptType
  case class TSTuple[E](of: Seq[TypescriptType])
      extends TypescriptAggregateType { def nested = of.toSet }
  object TSTuple {
    def of(of: TypescriptType*) = TSTuple(of)
  }
  case object TSUndefined extends TypescriptType
  case class TSUnion(of: Seq[TypescriptType]) extends TypescriptAggregateType {
    def nested = of.toSet
  }
  object TSUnion {
    def of(of: TypescriptType*) = TSUnion(of)
  }
  case object TSVoid extends TypescriptType

  sealed trait TSLiteral[T] extends TypescriptNamedType {
    val name: String
    val members: Seq[T]
  }

  case class TSLiteralString(name: String, members: Seq[String])
      extends TSLiteral[String]
  case class TSLiteralNumber[T: Numeric](name: String, members: Seq[T])
      extends TSLiteral[T]

  object TSLiteral {
    def apply(name: String, members: Seq[String]) =
      TSLiteralString(name, members)
    def apply[T: Numeric](name: String, members: Seq[T]) =
      TSLiteralNumber(name, members)

    def of(name: String, members: String*) =
      TSLiteralString(name, members)
    def of[T: Numeric](name: String, members: T*) =
      TSLiteralNumber(name, members)
  }

  private val tsIdentifierPattern = Pattern.compile(
    "[_$\\p{L}\\p{Nl}][_$\\p{L}\\p{Nl}\\p{Nd}\\{Mn}\\{Mc}\\{Pc}]*")
  private[interface] def isValidTSName(name: String): Boolean =
    tsIdentifierPattern.matcher(name).matches() && !reservedKeywords.contains(
      name)

  private[interface] final val reservedKeywords: Set[String] = Set(
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
