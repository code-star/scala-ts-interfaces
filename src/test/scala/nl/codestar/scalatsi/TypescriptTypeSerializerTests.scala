package nl.codestar.scalatsi

import nl.codestar.scalatsi.TypescriptType._
import nl.codestar.scalatsi.dsl._
import org.scalatest.{FlatSpec, Matchers}

class TypescriptTypeSerializerTests extends FlatSpec with Matchers with DefaultTSTypes {
  def ref(s: String) = TSRef(TSIdentifier(s), TSNamespace[TypescriptTypeSerializerTests])

  import org.scalactic._

  def whiteSpaceNormalised: Uniformity[String] =
    new AbstractStringUniformity {

      /** Returns the string with all consecutive white spaces reduced to a single space, then removes empty lines. */
      def normalized(s: String): String = s.replaceAll("\\s+", " ")

      override def toString: String = "whiteSpaceNormalised"
    }

  "The Typescript serializer" should "serialize to a simple interface" in {
    case class Person(name: String, age: Int)

    implicit val personTsWrites: TSIType[Person] = TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[Person]

    typescript.trim should equal("""export interface Person {
                                   |  name: string
                                   |  age: number
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "be able to generate multiple typescript interfaces for a nested case classes" in {
    case class ComplexCaseClass(nested: NestedCaseClass)
    case class NestedCaseClass(name: String)

    implicit val nestedCaseClassTSType: TSIType[NestedCaseClass]   = TSType.fromCaseClass[NestedCaseClass]
    implicit val complexCaseClassTSType: TSIType[ComplexCaseClass] = TSType.fromCaseClass[ComplexCaseClass]

    val typescript = TypescriptTypeSerializer.emit[ComplexCaseClass]

    typescript.trim should equal("""export interface NestedCaseClass {
                                   |  name: string
                                   |}
                                   |
                                   |export interface ComplexCaseClass {
                                   |  nested: NestedCaseClass
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "be able to handle options in a case class" in {
    case class OptionCaseClass(option: Option[String])

    implicit val optionCaseClassTSType: TSIType[OptionCaseClass] =
      TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[OptionCaseClass]

    typescript.trim should equal("""export interface OptionCaseClass {
                                   |  option?: string
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "handle recursive types" in {
    case class A(b: B)
    case class B(a: A)

    implicit val tsA: TSType[A]  = TSType.external("A")
    implicit val tsB: TSIType[B] = TSType.fromCaseClass
    val tsAGenerated: TSIType[A] = TSType.fromCaseClass

    TypescriptTypeSerializer
      .emit(tsAGenerated)
      .replaceAll("\\s", "") should equal("""
                                            |export interface B {
                                            |  a: A
                                            |}
                                            |
                                            |export interface A {
                                            |  b: B
                                            |}""".stripMargin.replaceAll("\\s", ""))
  }

  it should "be able to handle all primitive types" in {
    case class PrimitiveTypes(
      char: Char,
      string: String,
      byte: Byte,
      short: Short,
      int: Int,
      long: Long,
      float: Float,
      double: Double,
      boolean: Boolean,
      stringSeq: Seq[String]
    )

    implicit val primitiveTypesTSType: TSIType[PrimitiveTypes] =
      TSType.fromCaseClass[PrimitiveTypes]

    val typescript = TypescriptTypeSerializer.emit[PrimitiveTypes]

    typescript.trim should equal("""export interface PrimitiveTypes {
                                   |  char: number
                                   |  string: string
                                   |  byte: number
                                   |  short: number
                                   |  int: number
                                   |  long: number
                                   |  float: number
                                   |  double: number
                                   |  boolean: boolean
                                   |  stringSeq: string[]
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "serialize an indexed interface" in {
    case class Something(
      values: Map[String, String] = Map("a" -> "b")
    )

    implicit val somethingTSType: TSIType[Something] = TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[Something]

    typescript.trim should equal("""export interface Something {
                                   |  values: { [ key: string ]: string }
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "handle a type alias with nested types" in {
    val a    = TSAlias(ref("A"), TSNumber)
    val b    = TSAlias(ref("B"), TSString)
    val aOrB = TSAlias(ref("AOrB"), a | b)

    val typescript = TypescriptTypeSerializer.emit(aOrB).trim

    typescript should include("type A = number")
    typescript should include("type B = string")
    typescript should include("type AOrB = (A | B)")
  }

  it should "serialize a named indexed interface" in {
    case class Something(
      values: Map[String, String] = Map("a" -> "b")
    )

    implicit val somethingTSType: TSNamedType[Something] =
      TSType.interfaceIndexed[Something](indexName = "as", indexType = TSString, valueType = TSString)

    val typescript = TypescriptTypeSerializer.emit[Something]

    typescript.trim should equal("""export interface Something {
                                   |  [ as: string ]: string
                                   |}""".stripMargin)(after being whiteSpaceNormalised)
  }

  it should "handle string literal types" in {

    // How we define the Point in our typescript interface
    val expectedPoint = """export interface Point {
                          |  type: "Point"
                          |  coords: [number, number]
                          |}""".stripMargin

    // How we define the polygon in our typescript interface
    val expectedPolygon = """export interface Polygon {
                            |  type: "Polygon"
                            |  coords: [number, number][]
                            |}""".stripMargin

    sealed trait Geometry
    case class Point(lat: Double, lon: Double) extends Geometry
    case class Polygon(coords: Seq[Point])     extends Geometry

    implicit val pointTSType: TSNamedType[Point] =
      TSType.interface[Point]("type" -> ("Point": TypescriptType), "coords" -> classOf[(Double, Double)])
    implicit val polygonTSType: TSNamedType[Polygon] =
      TSType.interface[Polygon]("type" -> ("Polygon": TypescriptType), "coords" -> classOf[Seq[(Double, Double)]])
    implicit val geometryTSType: TSNamedType[Geometry] =
      TSType.alias[Geometry](implicitly[TSType[Point]] | implicitly[TSType[Polygon]])

    val typescript: String =
      TypescriptTypeSerializer.emits(implicitly[TSNamedType[Geometry]].get).trim

    TypescriptTypeSerializer.emit[Point].trim should equal(expectedPoint)
    typescript should include(expectedPoint)

    TypescriptTypeSerializer.emit[Polygon].trim should equal(expectedPolygon)
    typescript should include(expectedPolygon)

    typescript.trim should include("type Geometry = (Point | Polygon)")
  }

  it should "handle number literals" in {
    val expected  = "export type FourtyTwo = 42"
    val fourtyTwo = TSAlias(ref("FourtyTwo"), 42)

    val typescript = TypescriptTypeSerializer.emit(fourtyTwo).trim

    typescript should equal(expected)
  }

  it should "handle boolean literals" in {
    val expected = "export type MyBool = (true | false)"
    val myBool   = TSAlias(ref("MyBool"), (true: TypescriptType) | false)

    val typescript = TypescriptTypeSerializer.emit(myBool).trim

    typescript should equal(expected)
  }

  it should "handle object literals" in {
    val expected = "export type X = object"
    val x        = TSType.alias[Nothing, AnyRef].withName("X")
    val y        = TSType.alias[Nothing, Object].withName("X")

    TypescriptTypeSerializer.emit(x).trim should equal(expected)
    TypescriptTypeSerializer.emit(y).trim should equal(expected)
  }

}
