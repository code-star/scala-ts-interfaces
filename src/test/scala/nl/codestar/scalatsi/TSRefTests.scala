package nl.codestar.scalatsi

import org.scalatest.{FlatSpec, Matchers, TryValues}

class TSRefTests extends FlatSpec with Matchers with DefaultTSTypes with TryValues {
  "TSRef" should "convert a class into a typescript reference" in {
    TSRef[TSRefTests] shouldBe TSRef("nl.codestar.scalatsi.TSRefTests")
    TSRef[TSRefTests] shouldBe TSRef(TSIdentifier("TSRefTests"), TSNamespace("nl.codestar.scalatsi"))
  }

  "TSIdentifier" should "reject invalid identifiers" in {
    TSIdentifier("a&") should be theSameInstanceAs TSIdentifier.INVALID
    TSIdentifier("a-b") should be theSameInstanceAs TSIdentifier.INVALID
    TSIdentifier("0") should be theSameInstanceAs TSIdentifier.INVALID
    TSIdentifier("void") should be theSameInstanceAs TSIdentifier.INVALID
  }

  case class Foo(foo: String)
  it should "take nested classes into account" in {
    TSRef[Foo] shouldBe TSRef(TSIdentifier("Foo"), TSNamespace("nl", "codestar", "scalatsi", "TSRefTests"))
  }

  it should "use UNKNOWN for classes nested in anonymous classes" in {
    case class Nested(foo: String)
    TSRef[Nested] shouldBe TSRef(TSIdentifier("Nested"), TSNamespace.UNKNOWN)
  }

  "TSNamespace" should "parse a multipart namespace" in {
    TSNamespace("a").parts.map(_.id) shouldBe Seq("a")
    TSNamespace("a.b").parts.map(_.id) shouldBe Seq("a", "b")
    TSNamespace("a.b.c").parts.map(_.id) shouldBe Seq("a", "b", "c")
  }

  it should "convert a package to a namespace" in {
    TSNamespace[TSRefTests].parts.map(_.id) shouldBe Seq("nl", "codestar", "scalatsi")
  }

  it should "convert an invalid part to INVALID" in {
    TSNamespace("foo", "&123", "bar").parts.map(_.id) shouldBe Seq("foo", TSIdentifier.INVALID.id, "bar")
  }
}
