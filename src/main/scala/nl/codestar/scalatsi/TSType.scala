package nl.codestar.scalatsi

import nl.codestar.scalatsi.TypescriptType._

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe.WeakTypeTag

/* TODO: Move this somewhere to the docs
 * To define an implicit TSType[T]:
1. If the type maps directly to another type Other, use
    implicit val tsT: TSType[T] = TSType.Of[Other] //or
    implicit val tsT: TSType[T] = tsAlias[T, Other]
2. If T is a case class, use
    implicit val tsT: TSIType[T] = TSIType.fromCaseClass
3. Or use the DSL to build your own interface:
    import nl.codestar.scalatsi.dsl._
    implicit val tsT: TSIType[T] = tsInterface(
      "foo" -> classOf[String],
      "bar" -> classOf[Option[Int]]
    )
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

object TSType {
  private class TSTypeImpl[T](override val get: TypescriptType) extends TSType[T]
  def apply[T](tt: TypescriptType): TSType[T] = new TSTypeImpl(tt)

  /** Get an implicit `TSType[T]` */
  def get[T](implicit tsType: TSType[T]): TSType[T] = tsType

  /** Get an implicit `TSType[T]` or generate a default one
    *
    * By default
    * Case class will use [[fromCaseClass]]
    * Sealed traits/classes will use [[fromSealed]]
    * */
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
    * @see [Typescript docs on Discrimintated Unions](https://www.typescriptlang.org/docs/handbook/advanced-types.html#discriminated-unions)
    **/
  def fromSealed[T]: TSNamedType[T] = macro Macros.generateUnionFromSealedTrait[T]

  /** Uses the typescript type of Target whenever we're looking for the typescript type of Source
    * This will not generate a `type Source = Target` line like alias
    *
    * @see alias
    **/
  def sameAs[Source, Target](implicit tsType: TSType[Target]): TSType[Source] =
    TSType(tsType.get)

  /** Create a Typescript alias "name" for type T, with the definition of Alias
    *
    * @example alias[Foo, String]() will generate typescript `type Foo = string`
    * @see sameAs
    */
  def alias[T: WeakTypeTag, Alias]()(implicit tsType: TSType[Alias]): TSNamedType[T] =
    alias[T](tsType.get)

  /** Create a Typescript alias "name" for type T, with the definition of tsType
    *
    * @example alias[Foo](TSString) will generate typescript `type Foo = string`
    * @see sameAs
    */
  def alias[T: WeakTypeTag](tsType: TypescriptType): TSNamedType[T] =
    TSNamedType(TSAlias(TSRef[T], tsType))

  /** Create "name" as the typescript type for T, with "name" being defined elsewhere
    * external[Foo]("IXyz") will use "IXyz" as the typescript type every time something contains a Foo
    */
  def external[T](name: String): TSNamedType[T] =
    TypescriptType.fromString(name) match {
      case t: TSTypeReference => TSNamedType(t)
      case t                  => throw new IllegalArgumentException(s"String $name is a predefined type $t")
    }

  /** Create an interface "name" for T
    *
    * @example
    * interface[Foo]("bar" -> TSString) will output "interface Foo { bar: string }" in the package of Foo
    **/
  def interface[T: WeakTypeTag](members: (String, TypescriptType)*): TSIType[T] =
    TSIType(TSInterface(TSRef[T], members: _*))

  /** Create an indexed interface for T
    *
    * @example interfaceIndexed[Foo]("key", TSString, TSInt) will output "interface Foo { [key: string] : Int }"
    */
  def interfaceIndexed[T: WeakTypeTag](
    indexName: String = "key",
    indexType: TypescriptType = TSString,
    valueType: TypescriptType
  ): TSNamedType[T] =
    TSNamedType(TSNamedIndexedInterface(TSRef[T], TSIndexedInterface(indexName, indexType, valueType)))
}

@implicitNotFound(
  "Could not find an implicit TSNamedType[${T}] in scope. Make sure you created and imported a named typescript mapping for the type."
)
trait TSNamedType[T] extends TSType[T] {
  def get: TypescriptNamedType

  def withName(name: String): TSNamedType[T]           = TSNamedType(get.withName(name))
  def withNamespace(namespace: String): TSNamedType[T] = TSNamedType(get.withNamespace(namespace))

  override def toString: String = s"TSNamedType($get)"
}

object TSNamedType {
  private class TSNamedTypeImpl[T](override val get: TypescriptNamedType) extends TSNamedType[T]
  def apply[T](tt: TypescriptNamedType): TSNamedType[T] =
    new TSNamedTypeImpl(tt)

  /** Get an implicit `TSNamedType[T]` */
  def get[T](implicit tsType: TSNamedType[T]): TSNamedType[T] = tsType

  /** Get an implicit `TSNamedType[T]` or generate a default one
    *
    * @see [[TSType.getOrGenerate]]
    **/
  def getOrGenerate[T]: TSNamedType[T] = macro Macros.getImplicitMappingOrGenerateDefault[T, TSNamedType]
}

@implicitNotFound(
  "Could not find an implicit TSIType[${T}] in scope. Make sure you created and imported a typescript interface for the type."
)
trait TSIType[T] extends TSNamedType[T] { self =>
  override def get: TSInterface
  override def toString: String = s"TSIType($get)"
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
