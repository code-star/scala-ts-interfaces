package nl.codestar.scala.ts.interface

import nl.codestar.scala.ts.interface.TypescriptType._

trait DefaultTSTypes
    extends PrimitiveTSTypes
    with GenericTSTypes
    with TupleTSTypes
    with JavaTSTypes {

  implicit def seqTsType[E](implicit e: TSType[E]): TSType[Seq[E]] =
    TSType(TSArray(e.get))
  implicit def optionTsType[E](implicit e: TSType[E]): TSType[Option[E]] =
    TSType(TSUnion.of(e.get, TSUndefined))
}
object DefaultTSTypes extends DefaultTSTypes

trait PrimitiveTSTypes {
  implicit val booleanTsType: TSType[Boolean] = TSType(TSBoolean)
  implicit val stringTsType: TSType[String] = TSType(TSString)
  implicit def numberTsType[T: Numeric]: TSType[T] = TSType(TSNumber)
}
object PrimitiveTSTypes extends PrimitiveTSTypes

trait GenericTSTypes {
  // All scala collection types implement Traversable and are almost always translated to javascript arrays
  implicit def tsTraversable[E](implicit e: TSType[E]): TSType[Traversable[E]] =
    TSType(TSArray(e.get))
  // This chooses null union to represent Option types.
  // When defining interfaces however Option will be represented with undefined union
  implicit def tsOption[E](implicit e: TSType[E]): TSType[Option[E]] =
    TSType(TSUnion.of(e.get, TSNull))

  implicit def tsMap[E](implicit e: TSType[E]): TSType[Map[String, E]] =
    TSType(TSIndexedInterface(indexType = TSString, valueType = e.get))
}

trait JavaTSTypes {
  // Most JSON serializers write java.time times to a ISO8601-like string
  // Epoch (milli)seconds are also common, in this case users will need to provide their own TSType[TheirTimeRepresentation]
  // Should regex typescript types be implemented (https://github.com/Microsoft/TypeScript/issues/6579),
  // we could define more specific formats for the varying dates and times
  implicit val Java8DataTSType: TSType[java.time.temporal.Temporal] = TSType(
    TSString)

  // All java collection types implement Collection and are almost always translated to javascript arrays
  implicit def tsJavaCollection[E](
      implicit e: TSType[E]): TSType[java.util.Collection[E]] =
    TSType(TSArray(e.get))

  implicit val uriTSType: TSType[java.net.URI] = TSType(TSString)
  implicit val urlTSType: TSType[java.net.URL] = TSType(TSString)
  implicit val uuidTSType: TSType[java.util.UUID] = TSType(TSString)
}

trait TupleTSTypes {
  implicit def tsTuple1[T1](implicit t1: TSType[T1]): TSType[Tuple1[T1]] =
    TSType(TSTuple.of(t1.get))
  implicit def tsTuple2[T1, T2](implicit t1: TSType[T1],
                                t2: TSType[T2]): TSType[(T1, T2)] =
    TSType(TSTuple.of(t1.get, t2.get))
  implicit def tsTuple3[T1, T2, T3](implicit t1: TSType[T1],
                                    t2: TSType[T2],
                                    t3: TSType[T3]): TSType[(T1, T2, T3)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get))
  implicit def tsTuple4[T1, T2, T3, T4](
      implicit t1: TSType[T1],
      t2: TSType[T2],
      t3: TSType[T3],
      t4: TSType[T4]): TSType[(T1, T2, T3, T4)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get))
  implicit def tsTuple5[T1, T2, T3, T4, T5](
      implicit t1: TSType[T1],
      t2: TSType[T2],
      t3: TSType[T3],
      t4: TSType[T4],
      t5: TSType[T5]): TSType[(T1, T2, T3, T4, T5)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get, t5.get))
  implicit def tsTuple6[T1, T2, T3, T4, T5, T6](
      implicit t1: TSType[T1],
      t2: TSType[T2],
      t3: TSType[T3],
      t4: TSType[T4],
      t5: TSType[T5],
      t6: TSType[T6]): TSType[(T1, T2, T3, T4, T5, T6)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get, t5.get, t6.get))
  // TODO: Tuple7-21
}
