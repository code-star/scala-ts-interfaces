package nl.codestar.scalatsi

import org.scalatest.{FlatSpec, Matchers}
import nl.codestar.scalatsi.TypescriptType._

case class Person(name: String, age: Int)

class MacroTests extends FlatSpec with Matchers with DefaultTSTypes {
  def ref(s: String) = TSRef(TSIdentifier(s), TSNamespace[MacroTests] append TSIdentifier("MacroTests"))

  "The case class to TypeScript type macro" should "be able to translate a simple case class" in {
    case class Person(name: String, age: Int)
    TSType.fromCaseClass[Person].get shouldBe TSInterface(ref("Person"), "name" -> TSString, "age" -> TSNumber)
  }

  it should "handle optional types" in {
    case class TestOptional(opt: Option[Long])
    TSType.fromCaseClass[TestOptional].get shouldBe TSInterface(ref("TestOptional"), "opt" -> TSUnion.of(TSNumber, TSUndefined))
  }

  it should "handle nested definitions" in {
    case class A(foo: Boolean)
    case class B(a: A)

    val tsA: TSIType[A] = TSType.fromCaseClass

    TSType.fromCaseClass[B].get shouldBe TSInterface(ref("B"), "a" -> tsA.get)
  }

  "The sealed trait/class to Typescript type macro" should "handle sealed traits" in {
    sealed trait FooOrBar
    case class Foo(foo: String) extends FooOrBar
    case class Bar(bar: Int)    extends FooOrBar

    implicit val tsFoo = TSType.fromCaseClass[Foo]
    implicit val tsBar = TSType.fromCaseClass[Bar]

    // TODO: This will probably not emit Foo or Bar, when used, test this

    TSType.fromSealed[FooOrBar].get shouldBe TSAlias(ref("FooOrBar"), tsFoo.get.asReference | tsBar.get.asReference)
  }

  it should "handle sealed abstract classes" in {
    sealed abstract class FooOrBar(tpe: String)
    case class Foo(foo: String) extends FooOrBar("Foo")
    case class Bar(bar: Int)    extends FooOrBar("Bar")

    import nl.codestar.scalatsi.dsl._
    implicit val tsFoo = TSType.fromCaseClass[Foo] + ("type" -> "Foo")
    implicit val tsBar = TSType.fromCaseClass[Bar] + ("type" -> "Bar")

    tsFoo.get shouldBe TSInterface(ref("Foo"), "foo" -> TSString, "type" -> TSLiteralString("Foo"))
    tsBar.get shouldBe TSInterface(ref("Bar"), "bar" -> TSNumber, "type" -> TSLiteralString("Bar"))

    TSType.fromSealed[FooOrBar].get shouldBe TSAlias(ref("FooOrBar"), tsFoo.get.asReference | tsBar.get.asReference)
  }

  it should "handle sealed traits with a non-named mapping" in {
    sealed trait FooOrBar
    case class Foo(foo: String) extends FooOrBar
    case class Bar(bar: Int)    extends FooOrBar

    implicit val tsFoo = TSType.fromCaseClass[Foo]
    implicit val tsBar = TSType.sameAs[Bar, Int]

    tsFoo.get shouldBe TSInterface(ref("Foo"), "foo" -> TSString)
    tsBar.get shouldBe TSNumber

    TSType.fromSealed[FooOrBar].get shouldBe TSAlias(ref("FooOrBar"), tsFoo.get.asReference | TSNumber)
  }

  // Package cannot be inferred when the type is defined anonymously
  sealed trait LinkedList
  case object Nil                                     extends LinkedList
  case class Node(value: Int, next: LinkedList = Nil) extends LinkedList

  it should "handle sealed traits with recursive definitions" in {

    implicit val nilType: TSType[Nil.type] = TSType(TSNull)
    implicit val llType: TSNamedType[Node] = TSType.alias[Node](TSNull | TSTypeReference(ref("LinkedList")))

    val fromSealed = TSType.fromSealed[LinkedList]
    val expected   = TSType.alias[LinkedList](TSNull | TSTypeReference(ref("Node")))

    llType.get.ref shouldBe ref("Node")
    fromSealed.get.ref shouldBe ref("LinkedList")

    fromSealed shouldBe expected
  }

  it should "handle sealed traits without subclasses" in {
    sealed trait Empty

    // Expect a warning here
    TSType.fromSealed[Empty] shouldBe TSNamedType[Empty](TSAlias(ref("Empty"), TSNever))
  }

  it should "handle sealed traits with a single subclass" in {
    sealed trait Single
    case class A(foo: Int) extends Single

    TSType.fromSealed[Single] shouldBe TSType.alias[Single](TSTypeReference(ref("A")))
  }

  "TSType.getOrGenerate" should "use available implicit if in scope" in {
    case class A(foo: String)

    implicit val tsA = TSType[A](TSNumber)

    TSType.getOrGenerate[A] shouldBe theSameInstanceAs(tsA)
  }

  it should "use available implicit TSNamedType if in scope" in {
    case class A(foo: String)

    import dsl._
    implicit val tsA: TSNamedType[A] = TSType.fromCaseClass[A] + ("type" -> "A")

    TSNamedType.getOrGenerate[A] shouldBe TSType.interface[A](
      "foo"  -> TSString,
      "type" -> TSLiteralString("A")
    )
  }

  it should "use case class generator for case classes" in {
    case class A(foo: String)

    val generated     = TSType.getOrGenerate[A]
    val fromCaseClass = TSType.fromCaseClass[A]

    generated shouldBe fromCaseClass
    fromCaseClass shouldBe TSType.interface[A]("foo" -> TSString)
  }

  it should "use sealed trait generator for sealed traits" in {
    sealed trait A
    case class B(foo: String) extends A

    val generated  = TSType.getOrGenerate[A]
    val fromSealed = TSType.fromSealed[A]

    generated shouldBe fromSealed
    fromSealed shouldBe TSType.alias[A](TSTypeReference(ref("B")))
  }

  it should "give a compile error for unsupported types if no implicit is available" in {
    class A

    "TSType.getOrGenerate[A]" shouldNot compile
  }

  "TSIType.getOrGenerate" should "use available implicit if in scope" in {
    case class A(foo: String)

    implicit val tsA: TSIType[A] = TSType.interface[A]("bar" -> TSNumber)

    TSType.getOrGenerate[A] shouldBe theSameInstanceAs(tsA)
  }

  it should "use case class generator for case classes" in {
    case class A(foo: String)

    val generated     = TSIType.getOrGenerate[A]
    val fromCaseClass = TSType.fromCaseClass[A]

    generated shouldBe fromCaseClass
    fromCaseClass shouldBe TSType.interface[A]("foo" -> TSString)
  }

  it should "give a compile error for unsupported types if no implicit is available" in {
    sealed trait A

    "TSIType.getOrGenerate[A]" shouldNot compile
  }
}
