package zio.schema

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

  val anyOptional: Gen[Random with Sized, Schema.Optional[_]] =
    anySchema.map(Schema.Optional(_))

  val anyTuple: Gen[Random with Sized, Schema.Tuple[_, _]] =
    anySchema.zipWith(anySchema) { (a, b) =>
      Schema.Tuple(a, b)
    }

  val anySequence: Gen[Random with Sized, Schema.Sequence[_]] =
    anySchema.map(Schema.Sequence(_))

  val anyEnumeration: Gen[Random with Sized, Schema.Enumeration] =
    Gen.mapOf(Gen.anyString, anySchema).map(Schema.Enumeration)

  val anyRecord: Gen[Random with Sized, Schema.Record] =
    Gen.mapOf(Gen.anyString, anySchema).map(Schema.Record)

  val anyTransform: Gen[Random with Sized, Schema.Transform[_, _]] = ???

  lazy val anySchema: Gen[Random with Sized, Schema[_]] = Gen.oneOf(
    anyPrimitive,
    anyOptional,
    anyTuple,
    anySequence,
    anyEnumeration,
    anyRecord,
    anyTransform
  )
}
