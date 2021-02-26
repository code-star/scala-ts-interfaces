package com.scalatsi

import TypescriptType._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultTSTypeTests extends AnyWordSpec with Matchers with DefaultTSTypes {

  "Default TS Types should be defined" forWord {
    "String" in { "implicitly[TSType[String]]" should compile }
    "Boolean" in { "implicitly[TSType[Boolean]]" should compile }
    "Int" in { "implicitly[TSType[Int]]" should compile }
    "Long" in { "implicitly[TSType[Long]]" should compile }
    "Double" in { "implicitly[TSType[Double]]" should compile }
    "scala.math.BigDecimal" in { "implicitly[TSType[scala.math.BigDecimal]]" should compile }
    "java.math.BigDecimal" in { "implicitly[TSType[java.math.BigDecimal]]" should compile }
    "scala.math.BigInt" in { "implicitly[TSType[scala.math.BigInt]]" should compile }

    "Tuple2" in {
      "implicitly[TSType[(Int, String)]]" should compile
      val generated = implicitly[TSType[(Int, String)]].get
      val manual    = TSTuple.of(implicitly[TSType[Int]].get, implicitly[TSType[String]].get)
      generated should ===(manual)
    }

    "Tuple3" in {
      "implicitly[TSType[(Int, String, Double)]]" should compile
      val generated = implicitly[TSType[(Int, String, Double)]].get
      val manual    = TSTuple.of(implicitly[TSType[Int]].get, implicitly[TSType[String]].get, implicitly[TSType[Double]].get)
      generated should ===(manual)
    }

    "None" in {
      "implicitly[TSType[None.type]]" should compile
      implicitly[TSType[None.type]].get shouldBe TSNull
    }

    "Option" in {
      "implicitly[TSType[Option[Int]]]" should compile
      val generated = implicitly[TSType[Option[Int]]].get
      val manual    = implicitly[TSType[Int]] | TSUndefined
      generated should ===(manual)
    }

    "Some" in {
      implicitly[TSType[Some[Int]]] shouldBe implicitly[TSType[Int]]
    }

    "Either" in {
      "implicitly[TSType[Either[Int, String]]]" should compile
      val generated = implicitly[TSType[Either[Int, String]]].get
      val manual    = implicitly[TSType[Int]] | implicitly[TSType[String]]
      generated should ===(manual)
    }

    "Map[String, _]" in {
      "implicitly[TSType[Map[String, Int]]]" should compile
    }

    "Map[Int, _]" in {
      "implicitly[TSType[Map[Int, Int]]]" should compile
    }

    "Seq[_]" in {
      "implicitly[TSType[scala.collection.mutable.Seq[Int]]]" should compile
      "implicitly[TSType[scala.collection.immutable.Seq[Int]]]" should compile
      "implicitly[TSType[IndexedSeq[Int]]]" should compile
      "implicitly[TSType[List[Int]]]" should compile
      "implicitly[TSType[Vector[Int]]]" should compile
    }

    "Set[_]" in {
      "implicitly[TSType[Set[Int]]]" should compile
      "implicitly[TSType[scala.collection.immutable.HashSet[Int]]]" should compile
    }

    "Collection<?>" in {
      "implicitly[TSType[java.util.Collection[Int]]]" should compile
      "implicitly[TSType[java.util.List[Int]]]" should compile
      "implicitly[TSType[java.util.ArrayList[Int]]]" should compile
    }

    "Function" in {
      "implicitly[TSType[Function0[Int]]]" should compile
      "implicitly[TSType[String => String]]" should compile
      "implicitly[TSType[(Int, String) => Unit]]" should compile
      "implicitly[TSType[(Int, Int, Int) => Int]]" should compile
    }
  }
}
