package com.scalatsi

import TypescriptType._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait ScalaTSTypes {
  implicit val anyTSType: TSType[Any] = TSType(TSAny)
}

trait CollectionTSTypes extends LowPriorityCollectionTSType {

  /** Represent an Option[E] with a Typescript (E | undefined) */
  implicit def tsOption[E](implicit e: TSType[E]): TSType[Option[E]] = TSType(e | TSUndefined)
  implicit val noneTSType: TSType[None.type]                         = TSType(TSNull)
  implicit def tsSome[E](implicit e: TSType[E]): TSType[Some[E]]     = TSType(e.get)

  implicit def tsEither[L, R](implicit tsLeft: TSType[L], tsRight: TSType[R]): TSType[Either[L, R]] = TSType(tsLeft | tsRight)

  implicit def tsStringMap[E](implicit e: TSType[E]): TSType[Map[String, E]] =
    TSType(TSIndexedInterface(indexType = TSString, valueType = e.get))

  implicit def tsIntMap[E](implicit e: TSType[E]): TSType[Map[Int, E]] =
    TSType(TSIndexedInterface(indexType = TSNumber, valueType = e.get))
}

trait LowPriorityCollectionTSType {

  /** Provides a TSType for any scala collection of E to a typescript array of E */
  implicit def tsTraversable[E, F[_]](implicit e: TSType[E], ev: F[E] <:< Iterable[E]): TSType[F[E]] = TSType(e.get.array)
}

trait ScalaEnumTSTypes {
  implicit def scalaEnumTSType[E <: Enumeration: TypeTag]: TSNamedType[E] = {
    // we have to use reflection to call the enum values() method, no easy way to get the object directly in Scala 2.12
    // might be possible to improve using 2.13+ singleton types
    val tpe  = scala.reflect.runtime.universe.typeOf[E]
    val name = tpe.typeSymbol.name.toString
    val members = tpe.members.filter(sym =>
      // For some reason the types here are not subtypes of E#Value or Enumeration#Value (=:= and <:< return false)
      // Instead filter out everything but "private[this] val"s. Those shouldn't be in Enumeration's anyway
      sym.isPrivateThis &&
        !sym.isMethod &&
        !sym.isClass &&
        !sym.isType &&
        !sym.isModule
    )

    val values = members
      .map(_.name.toString.trim)
      .toSeq
      .sorted

    TSType.alias[E](name, TSUnion(values.map(TSLiteralString.apply)))
  }
}

trait JavaTSTypes {
  implicit val javaObjectTSType: TSType[Object] = TSType(TSObject)

  import java.time.temporal.Temporal
  // Most JSON serializers write java.time times to a ISO8601-like string
  // Epoch (milli)seconds are also common, in this case users will need to provide their own TSType[TheirTimeRepresentation]
  // Should regex typescript types be implemented (https://github.com/Microsoft/TypeScript/issues/6579),
  // we could define more specific formats for the varying dates and times
  /** Type to serialize java.time.* dates/times and java.util.Date to, override this to change your representation */
  protected val java8TimeTSType: TSType[Temporal]     = TSType(TSString)
  implicit val javaDateTSType: TSType[java.util.Date] = java8TimeTSType.asInstanceOf[TSType[java.util.Date]]

  implicit def java8DateTSTypeConversion[T <: Temporal]: TSType[T] = java8TimeTSType.asInstanceOf[TSType[T]]

  implicit def javaNumber[T <: java.lang.Number]: TSType[T] = TSType(TSNumber)

  // All java collection types implement Collection and are almost always translated to javascript arrays
  implicit def tsJavaCollection[E, F[_]](implicit
    e: TSType[E],
    ev: F[E] <:< java.util.Collection[E]
  ): TSType[F[E]] = TSType(e.get.array)

  implicit val javaUriTSType: TSType[java.net.URI]    = TSType(TSString)
  implicit val javaUrlTSType: TSType[java.net.URL]    = TSType(TSString)
  implicit val javaUuidTSType: TSType[java.util.UUID] = TSType(TSString)

  implicit def javaEnumTSType[E <: java.lang.Enum[E]: ClassTag]: TSNamedType[E] = {

    val cls = implicitly[ClassTag[E]].runtimeClass
    val values = Option(cls.getEnumConstants)
      .getOrElse(throw new IllegalStateException(s"Expected ${cls.getCanonicalName} to be a java.lang.Enum, it was not"))
      .asInstanceOf[Array[E]]
      .toSeq

    TSType.alias[E](
      cls.getSimpleName,
      TSUnion(values.map(v => TSLiteralString(v.name())))
    )
  }
}

trait TupleTSTypes {
  implicit def tsTuple1[T1](implicit t1: TSType[T1]): TSType[Tuple1[T1]] =
    TSType(TSTuple.of(t1.get))
  implicit def tsTuple2[T1, T2](implicit t1: TSType[T1], t2: TSType[T2]): TSType[(T1, T2)] =
    TSType(TSTuple.of(t1.get, t2.get))
  implicit def tsTuple3[T1, T2, T3](implicit t1: TSType[T1], t2: TSType[T2], t3: TSType[T3]): TSType[(T1, T2, T3)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get))
  implicit def tsTuple4[T1, T2, T3, T4](implicit t1: TSType[T1], t2: TSType[T2], t3: TSType[T3], t4: TSType[T4]): TSType[(T1, T2, T3, T4)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get))
  implicit def tsTuple5[T1, T2, T3, T4, T5](implicit
    t1: TSType[T1],
    t2: TSType[T2],
    t3: TSType[T3],
    t4: TSType[T4],
    t5: TSType[T5]
  ): TSType[(T1, T2, T3, T4, T5)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get, t5.get))
  implicit def tsTuple6[T1, T2, T3, T4, T5, T6](implicit
    t1: TSType[T1],
    t2: TSType[T2],
    t3: TSType[T3],
    t4: TSType[T4],
    t5: TSType[T5],
    t6: TSType[T6]
  ): TSType[(T1, T2, T3, T4, T5, T6)] =
    TSType(TSTuple.of(t1.get, t2.get, t3.get, t4.get, t5.get, t6.get))
  // TODO: Tuple7-21
}

trait FunctionTSTypes {
  implicit def tsFunction0[R](implicit r: TSType[R]): TSType[() => R] =
    TSType(TSFunction(List(), r.get))

  implicit def tsFunction1[T1, R](implicit t1: TSType[T1], r: TSType[R]): TSType[T1 => R] =
    TSType(
      TSFunction(
        List(
          ("arg1", t1.get) // TODO: use reflection to get the real argument name
        ),
        r.get
      )
    )

  implicit def tsFunction2[T1, T2, R](implicit t1: TSType[T1], t2: TSType[T2], r: TSType[R]): TSType[(T1, T2) => R] =
    TSType(
      TSFunction(
        List(
          ("arg1", t1.get),
          ("arg2", t2.get)
        ),
        r.get
      )
    )

  implicit def tsFunction3[T1, T2, T3, R](implicit
    t1: TSType[T1],
    t2: TSType[T2],
    t3: TSType[T3],
    r: TSType[R]
  ): TSType[(T1, T2, T3) => R] =
    TSType(
      TSFunction(
        List(
          ("arg1", t1.get),
          ("arg2", t2.get),
          ("arg3", t3.get)
        ),
        r.get
      )
    )

  // TODO: Function4-22
}
