package com.scalatsi

import TypescriptType._

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

/** A typeclass that indicates what the Typescript equivalent of a type T is
  * See the methods in the TSType object and the [[dsl]] for how to construct it
  * @see [[TSType.getOrGenerate]]
  */
@implicitNotFound("Could not find an implicit TSType[${T}] in scope. Did you create and import it?")
trait TSType[T] { self =>
  def get: TypescriptType
  override def equals(obj: scala.Any): Boolean = obj match {
    case o: TSType[_] => get == o.get
    case _            => false
  }
  override def hashCode(): Int  = get.hashCode()
  override def toString: String = s"TSType($get)"

  // Forwarders to the underlying TypescriptType
  def |(other: TypescriptType): TSUnion = get | other
  def |(other: TSType[_]): TSUnion      = this | other.get
}

object TSType extends DefaultTSTypes {
  private class TSTypeImpl[T](override val get: TypescriptType) extends TSType[T]
  def apply[T](tt: TypescriptType): TSType[T] = new TSTypeImpl(tt)

  def named[T](tt: TypescriptNamedType): TSNamedType[T] = TSNamedType[T](tt)

  /** Get an implicit `TSType[T]` */
  def get[T](implicit tsType: TSType[T]): TSType[T] = tsType

  /** Get an implicit `TSType[T]` or generate a default one
    *
    * By default
    * Case class will use [[fromCaseClass]]
    * Sealed traits/classes will use [[fromSealed]]
    */
  def getOrGenerate[T]: TSType[T] = macro Macros.getImplicitMappingOrGenerateDefault[T, TSType]

  /** Generate a typescript interface for a case class */
  def fromCaseClass[T]: TSIType[T] = macro Macros.generateInterfaceFromCaseClass[T]

  /** Generate a Typescript discriminated union from a scala sealed trait
    *
    * @example
    * ```
    * sealed trait AorB
    * case class A(foo: Int) extends AorB
    * case class B(bar: Int) extends AorB
    *
    * implicit val tsAorB = TSType.fromSealed[AorB]
    * ```
    *
    * wil produce
    *
    * `type AorB = A | B`
    * @see [Typescript docs on Discriminated Unions](https://www.typescriptlang.org/docs/handbook/unions-and-intersections.html#discriminating-unions)
    */
  def fromSealed[T]: TSNamedType[T] = macro Macros.generateUnionFromSealedTrait[T]

  /** Uses the typescript type of Target whenever we're looking for the typescript type of Source
    * This will not generate a `type Source = Target` line like alias
    *
    * @see alias
    */
  def sameAs[Source, Target](implicit tsType: TSType[Target]): TSType[Source] = TSType(tsType.get)

  /** Create a Typescript alias "T" for type T, with the definition of Alias
    *
    * @example alias[Foo, String] will generate typescript `type Foo = string`
    * @see sameAs
    */
  def alias[T, Alias](implicit tsType: TSType[Alias], ct: Manifest[T]): TSNamedType[T] =
    alias[T, Alias](ct.runtimeClass.getSimpleName)

  /** Create a Typescript alias "name" for type T, with the definition of Alias
    *
    * @example alias[Foo, String]("IFoo") will generate typescript `type IFoo = string`
    * @see sameAs
    */
  def alias[T, Alias](name: String)(implicit tsType: TSType[Alias]): TSNamedType[T] =
    alias(name, tsType.get)

  /** Create a Typescript alias "name" for type T, with the definition of tsType
    *
    * @example alias[Foo]("IFoo", TSString) will generate typescript `type IFoo = string`
    * @see sameAs
    */
  def alias[T](name: String, tsType: TypescriptType): TSNamedType[T] =
    TSNamedType(TSAlias(name, tsType))

  /** Create "name" as the typescript type for T, with "name" being defined elsewhere
    * external[Foo]("IXyz") will use "IXyz" as the typescript type every time something contains a Foo
    */
  def external[T](name: String): TSNamedType[T] =
    TypescriptType.fromString(name) match {
      case t: TSTypeReference => TSNamedType(t)
      case t =>
        throw new IllegalArgumentException(s"String $name is a predefined type $t")
    }

  /** Create an interface "name" for T
    *
    * @example interface[Foo]("MyFoo", "bar" -> TSString) will output "interface MyFoo { bar: string }"
    */
  def interface[T](name: String, members: (String, TypescriptType)*): TSIType[T] =
    TSIType(TSInterface(name, ListMap(members: _*)))

  /** Create an interface "IClassname" for T
    *
    * @example interface[Foo]("bar" -> TSString) will output "interface IFoo { bar: string }"
    */
  @deprecated("0.2.3", "Use interface(name, ...), this method confused the Scala overload resolver")
  def interface[T](members: (String, TypescriptType)*)(implicit ct: Manifest[T]): TSIType[T] =
    interface[T]("I" + ct.runtimeClass.getSimpleName, members: _*)

  /** Create an indexed interface for T
    *
    * @example interfaceIndexed[Foo]("IFooLookup", "key", TSString, TSInt) will output "interface IFooLookup { [key: string] : Int }"
    */
  def interfaceIndexed[T](
    name: String,
    indexName: String = "key",
    indexType: TypescriptType = TSString,
    valueType: TypescriptType
  ): TSNamedType[T] =
    TSNamedType(TSInterfaceIndexed(name, indexName, indexType, valueType))

  /** Create a tagged primitive type. This can be used in combination with popular type tagging solutions as offered by
    * Shapeles, Scala-common or Scalaz.
    *
    * Usage example 1:
    *
    * import shapeless.tag.@@
    *
    * sealed trait Name
    * implicit val tsName: TSType[String @@ Name] = TSType.taggedPrimitive[@@, String, Name]()
    *
    * However, it is more common to define a new type with the actual type name and a separately named Tag. In this case
    * we need to define a naming convention that describes how the tag name relates to the actual type name:
    *
    * Usage example 2:
    *
    * import shapeless.tag.@@
    *
    * sealed trait NameTag
    * type Name = String @@ NameTag
    * implicit val tsName: TSType[Name] = TSType.taggedPrimitive[@@, String, NameTag](_.dropRight(3))
    *
    * Note: one could also make the tag argument generic and enable tagging for all tagged primitives:
    *
    * implicit def tsTaggedString[Tag: ClassTag]: TSType[String @@ Tag] =
    *   TSType.taggedPrimitive[@@, String, Tag](_.dropRight(3))
    */
  def taggedPrimitive[@@[_, _], P: ClassTag, T: ClassTag](namingConvention: String => String = s => s): TSNamedType[@@[P, T]] = {
    def getTypeName[TT: ClassTag]: String = implicitly[ClassTag[TT]].runtimeClass.getSimpleName
    val primitive = getTypeName[P] match {
      case "String"                            => TSString
      case "int" | "float" | "long" | "double" => TSNumber
      case t                                   => throw new RuntimeException(s"Cannot handle primitive type $t")
    }
    val tName = namingConvention(getTypeName[T])
    TSNamedType(TSAlias(tName, TSTaggedPrimitive(tName, primitive)))
  }
}

@implicitNotFound(
  "Could not find an implicit TSNamedType[${T}] in scope. Make sure you created and imported a named typescript mapping for the type."
)
trait TSNamedType[T] extends TSType[T] { self =>
  override def get: TypescriptNamedType
  override def toString: String = s"TSNamedType($get)"

  def withName(newName: String): TSNamedType[T] = TSNamedType(get.withName(newName))
}

object TSNamedType extends DefaultTSTypes {
  private class TSNamedTypeImpl[T](override val get: TypescriptNamedType) extends TSNamedType[T]
  def apply[T](tt: TypescriptNamedType): TSNamedType[T] =
    new TSNamedTypeImpl(tt)

  /** Get an implicit `TSNamedType[T]` */
  def get[T](implicit tsType: TSNamedType[T]): TSNamedType[T] = tsType

  /** Get an implicit `TSNamedType[T]` or generate a default one
    *
    * @see [[TSType.getOrGenerate]]
    */
  def getOrGenerate[T]: TSNamedType[T] = macro Macros.getImplicitMappingOrGenerateDefault[T, TSNamedType]

  implicit def ordering[T]: Ordering[TSNamedType[T]] = Ordering.by[TSNamedType[T], TypescriptNamedType](_.get)
}

@implicitNotFound(
  "Could not find an implicit TSIType[${T}] in scope. Make sure you created and imported a typescript interface for the type."
)
trait TSIType[T] extends TSNamedType[T] { self =>
  override def get: TSInterface
  override def toString: String                      = s"TSIType($get)"
  override def withName(newName: String): TSIType[T] = TSIType(get.withName(newName))
}

object TSIType {
  private class TSITypeImpl[T](override val get: TSInterface) extends TSIType[T]
  def apply[T](tt: TSInterface): TSIType[T] = new TSITypeImpl(tt)

  /** Get an implicit TSIType[T] */
  def get[T](implicit tsType: TSIType[T]): TSIType[T] = tsType

  /** Get an implicit `TSIType[T]` or generate a default one
    *
    * @see [[TSType.getOrGenerate]]
    */
  def getOrGenerate[T]: TSIType[T] = macro Macros.getImplicitInterfaceMappingOrGenerateDefault[T, TSIType]
}
