package zio.web.schema

import zio.random.Random
import zio.schema.Schema
import zio.test.{ Gen, Sized }

object SchemaGen {

  type SchemaAndValue[S[_] <: Schema[_], A] = (S[A], A)

  val anyPrimitive: Gen[Random, Schema.Primitive[_]] =
    StandardTypeGen.anyStandardType.map(Schema.Primitive(_))

  val anyPrimitiveAndValue: Gen[Random with Sized, SchemaAndValue[Schema.Primitive, _]] =
    StandardTypeGen.anyStandardTypeAndValue.map {
      case (standardType, value) => Schema.Primitive(standardType) -> value
    }
}
