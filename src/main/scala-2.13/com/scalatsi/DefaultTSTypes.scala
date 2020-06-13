package com.scalatsi

/** The default Scala-to-Typescript mappings provided by this library */
// Cannot be put in the scala-agnostic code because 2.12 does not support literal types
trait DefaultTSTypes
  extends PrimitiveTSTypes
  with ScalaTSTypes
  with CollectionTSTypes
  with TupleTSTypes
  with JavaTSTypes
  with LiteralTSTypes {}
object DefaultTSTypes extends DefaultTSTypes
