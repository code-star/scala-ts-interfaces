package com.scalatsi

import com.scalatsi.output.StyleOptions
import TypescriptType._
import dsl._

import scala.annotation.nowarn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypescriptTypeSerializerTests extends AnyFlatSpec with Matchers with DefaultTSTypes {

  "The Typescript serializer" should "serialize to a simple interface" in {
    case class Person(name: String, age: Int)

    implicit val personTsWrites: TSIType[Person] = TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[Person]()

    typescript should equal("""export interface IPerson {
                              |  name: string
                              |  age: number
                              |}
                              |""".stripMargin)
  }

  it should "serialize an interface with semicolons if configured" in {
    case class Person(name: String, age: Int)

    implicit val personTsWrites: TSIType[Person] = TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[Person](StyleOptions(semicolons = true))

    typescript should equal("""export interface IPerson {
                              |  name: string;
                              |  age: number;
                              |}
                              |""".stripMargin)
  }

  it should "be able to generate multiple typescript interfaces for a nested case classes" in {
    case class ComplexCaseClass(nested: NestedCaseClass)
    case class NestedCaseClass(name: String)

    @nowarn("cat=unused") implicit val nestedCaseClassTSType: TSIType[NestedCaseClass] = TSType.fromCaseClass[NestedCaseClass]
    implicit val complexCaseClassTSType: TSIType[ComplexCaseClass]                     = TSType.fromCaseClass[ComplexCaseClass]

    val typescript = TypescriptTypeSerializer.emit[ComplexCaseClass]()

    typescript should equal("""export interface IComplexCaseClass {
                              |  nested: INestedCaseClass
                              |}
                              |
                              |export interface INestedCaseClass {
                              |  name: string
                              |}
                              |""".stripMargin)
  }

  it should "be able to handle options in a case class" in {
    case class OptionCaseClass(option: Option[String])

    implicit val optionCaseClassTSType: TSIType[OptionCaseClass] =
      TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[OptionCaseClass]()

    typescript should equal("""export interface IOptionCaseClass {
                              |  option?: string
                              |}
                              |""".stripMargin)
  }

  it should "handle recursive types with a work-around" in {
    case class A(b: B)
    case class B(a: A)

    // Test work-around for circular references
    // see [[Macros.circularRefError]] and https://github.com/scala-tsi/scala-tsi#circular-references
    @nowarn("cat=unused") implicit val tsB: TSIType[B] = {
      implicit val tsA: TSType[A] = TSType.external("IA")
      TSType.fromCaseClass[B]
    }

    val tsAGenerated: TSIType[A] = TSType.fromCaseClass
    TypescriptTypeSerializer.emit()(tsAGenerated) should equal("""|export interface IA {
                                                                  |  b: IB
                                                                  |}
                                                                  |
                                                                  |export interface IB {
                                                                  |  a: IA
                                                                  |}
                                                                  |""".stripMargin)
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

    val typescript = TypescriptTypeSerializer.emit[PrimitiveTypes]()

    typescript should equal("""export interface IPrimitiveTypes {
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
                              |}
                              |""".stripMargin)
  }

  it should "serialize an indexed interface" in {
    case class Something(
      values: Map[String, String] = Map("a" -> "b")
    )

    implicit val somethingTSType: TSIType[Something] = TSType.fromCaseClass

    val typescript = TypescriptTypeSerializer.emit[Something]()

    typescript should equal("""export interface ISomething {
                              |  values: { [ key: string ]: string }
                              |}
                              |""".stripMargin)
  }

  it should "handle a type alias with nested types" in {
    val a    = TSType.alias("A", TSNumber)
    val b    = TSType.alias("B", TSString)
    val aOrB = TSType.alias("AOrB", a | b)

    val typescript = TypescriptTypeSerializer.emit()(aOrB).trim

    typescript should include("type A = number")
    typescript should include("type B = string")
    typescript should include("type AOrB = (A | B)")
  }

  it should "serialize a named indexed interface" in {
    case class Something(
      values: Map[String, String] = Map("a" -> "b")
    )

    implicit val somethingTSType: TSNamedType[Something] =
      TSType.interfaceIndexed(name = "ISomething", indexName = "as", indexType = TSString, valueType = TSString)

    val typescript = TypescriptTypeSerializer.emit[Something]()

    typescript should equal("""export interface ISomething {
                              |  [ as: string ]: string
                              |}
                              |""".stripMargin)
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

    // TODO: The type ascriptions can be removed once the deprecated TSType.interface is removed or 2.12 support is dropped
    implicit val pointTSType: TSNamedType[Point] =
      TSType.interface("Point", "type" -> ("Point": TypescriptType), "coords" -> classOf[(Double, Double)])
    implicit val polygonTSType: TSNamedType[Polygon] =
      TSType.interface("Polygon", "type" -> ("Polygon": TypescriptType), "coords" -> classOf[Seq[(Double, Double)]])
    implicit val geometryTSType: TSNamedType[Geometry] =
      TSType.alias("Geometry", implicitly[TSType[Point]] | implicitly[TSType[Polygon]])

    val typescript: String =
      TypescriptTypeSerializer.emits(implicitly[TSNamedType[Geometry]].get).trim

    TypescriptTypeSerializer.emit[Point]().trim should equal(expectedPoint)
    typescript should include(expectedPoint)

    TypescriptTypeSerializer.emit[Polygon]().trim should equal(expectedPolygon)
    typescript should include(expectedPolygon)

    typescript.trim should include("type Geometry = (Point | Polygon)")
  }

  it should "handle number literals" in {
    val expected  = "export type FourtyTwo = 42"
    val fourtyTwo = TSType.alias("FourtyTwo", 42)

    val typescript = TypescriptTypeSerializer.emit()(fourtyTwo).trim

    typescript should equal(expected)
  }

  it should "handle boolean literals" in {
    val expected = "export type MyBool = (true | false)"
    val myBool   = TSType.alias("MyBool", (true: TypescriptType) | false)

    val typescript = TypescriptTypeSerializer.emit()(myBool).trim

    typescript should equal(expected)
  }

  it should "handle object literals" in {
    val expected = "export type X = object"
    val x        = TSType.alias[Nothing, AnyRef]("X")
    val y        = TSType.alias[Nothing, Object]("X")

    TypescriptTypeSerializer.emit()(x).trim should equal(expected)
    TypescriptTypeSerializer.emit()(y).trim should equal(expected)
  }

  it should "serialize function members of interfaces" in {
    val interface =
      TSType.interface(
        "TestInterface",
        (
          "fun1",
          TSFunction(
            List(
              ("iarg", TSNumber),
              ("sarg", TSString)
            ),
            TSVoid
          )
        ),
        (
          "fun2",
          TSFunction(
            List(
              (
                "farg",
                TSFunction(
                  List(
                    ("iarg", TSNumber),
                    ("sarg", TSString)
                  ),
                  TSVoid
                )
              )
            ),
            TSNumber
          )
        )
      )

    val serialized = TypescriptTypeSerializer.emit()(interface).trim

    val expected =
      """export interface TestInterface {
        |  fun1(iarg: number, sarg: string): void
        |  fun2(farg: (iarg: number, sarg: string) => void): number
        |}""".stripMargin

    serialized should equal(expected)
  }
}
