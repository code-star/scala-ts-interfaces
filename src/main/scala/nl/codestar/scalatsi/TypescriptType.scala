package nl.codestar.scalatsi

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
      case _           => TSTypeReference(TSRef(tpe))
    }

  /** Get a reference to a named type, or the type itself if it is unnamed or built-in */
  def nameOrType(tpe: TypescriptType): TypescriptType = tpe match {
    case named: TypescriptNamedType => named.asReference
    case anonymous                  => anonymous
  }

  /** A marker trait for a TS type that has a name */
  sealed trait TypescriptNamedType extends TypescriptType {

    /** concrete implementation type (e.g. TSInterface) */
    type Self <: TypescriptNamedType

    /** A reference to the type */
    val ref: TSRef

    /** Return a reference to this type */
    def asReference: TSTypeReference = TSTypeReference(ref)

    /** Change the name of this type
      * @see [[TSIdentifier.apply]]
      **/
    def withName(name: String): Self = withRef(ref.withName(name))

    /** Change the namespace of this type
      * @see [[TSNamespace.apply]]*/
    def withNamespace(namespace: String): Self = withRef(ref.withNamespace(namespace))

    protected def withRef(ref: TSRef): Self
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
  final case class TSAlias(override val ref: TSRef, underlying: TypescriptType) extends TypescriptNamedType with TypescriptAggregateType {
    override type Self = TSAlias

    override def nested: Set[TypescriptType] = Set(underlying)

    override protected def withRef(ref: TSRef): TSAlias = this.copy(ref = ref)
  }

  case object TSAny                               extends TypescriptType
  case class TSArray(elementType: TypescriptType) extends TypescriptAggregateType { def nested: Set[TypescriptType] = Set(elementType) }
  case object TSBoolean                           extends TypescriptType

  sealed trait TSLiteralType[T]                 extends TypescriptType { val value: T }
  case class TSLiteralString(value: String)     extends TSLiteralType[String]
  case class TSLiteralNumber(value: BigDecimal) extends TSLiteralType[BigDecimal]
  case class TSLiteralBoolean(value: Boolean)   extends TSLiteralType[Boolean]

  case class TSEnum(ref: TSRef, const: Boolean, entries: ListMap[String, Option[Int]])
      extends TypescriptNamedType
      with TypescriptAggregateType {
    override type Self = TSEnum

    override def nested: Set[TypescriptType]           = Set(TSNumber)
    override protected def withRef(ref: TSRef): TSEnum = this.copy(ref = ref)
  }

  /** A reference to a typescript type
    * @see https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#3.8.2
    * */
  sealed case class TSTypeReference(ref: TSRef) extends TypescriptNamedType {
    override type Self = TSTypeReference
    override def asReference: TSTypeReference                   = this
    override protected def withRef(ref: TSRef): TSTypeReference = TSTypeReference(ref)
  }

  /** Typescript anonymous indexed interfaces
    * { [indexName:indexType]: valueType}
    * @param indexType index type, TSNumber or TSString
    **/
  case class TSIndexedInterface(indexName: String = "key", indexType: TypescriptType, valueType: TypescriptType)
      extends TypescriptAggregateType {
    require(
      indexType == TSString || indexType == TSNumber,
      s"TypeScript indexed interface can only have index type string or number, not $indexType"
    )
    def nested: Set[TypescriptType] = Set(indexType, valueType)
  }
  case class TSNamedIndexedInterface(ref: TSRef, interface: TSIndexedInterface) extends TypescriptNamedType with TypescriptAggregateType {
    override type Self = TSNamedIndexedInterface

    override def nested: Set[TypescriptType]                            = interface.nested
    override protected def withRef(ref: TSRef): TSNamedIndexedInterface = this.copy(ref = ref)
  }

  case class TSInterface private (ref: TSRef, members: ListMap[String, TypescriptType])
      extends TypescriptNamedType
      with TypescriptAggregateType {
    override type Self = TSInterface

    override def nested: Set[TypescriptType]                = members.values.toSet
    override protected def withRef(ref: TSRef): TSInterface = this.copy(ref = ref)
  }
  object TSInterface {
    def apply(ref: TSRef, members: ListMap[String, TypescriptType]): TSInterface = new TSInterface(ref, members)
    def apply(ref: TSRef, members: (String, TypescriptType)*): TSInterface       = TSInterface(ref, ListMap(members: _*))
  }
  case class TSIntersection(of: Seq[TypescriptType]) extends TypescriptAggregateType { def nested: Set[TypescriptType] = of.toSet }
  object TSIntersection {
    def of(of: TypescriptType*) = TSIntersection(of)
  }
  case object TSNever  extends TypescriptType
  case object TSNull   extends TypescriptType
  case object TSNumber extends TypescriptType
  case object TSObject extends TypescriptType
  case object TSString extends TypescriptType

  /** Typescript tuple: `[0.type, 1.type, ... n.type]` */
  case class TSTuple[E](of: Seq[TypescriptType]) extends TypescriptAggregateType { def nested: Set[TypescriptType] = of.toSet }
  object TSTuple {
    def of(of: TypescriptType*) = TSTuple(of.to(Seq))
  }

  case object TSUndefined extends TypescriptType
  case class TSUnion(of: Seq[TypescriptType]) extends TypescriptAggregateType {
    def nested: Set[TypescriptType] = of.toSet
  }
  object TSUnion {
    def of(of: TypescriptType*) = TSUnion(of.toSeq)
  }
  case object TSVoid extends TypescriptType

}
