package zio.schema

import zio.Chunk
import zio.random.Random
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

  type SequenceTransform[A] = Schema.Transform[Chunk[A], List[A]]

  val anySequenceTransform: Gen[Random with Sized, SequenceTransform[_]] = {
    // TODO: Add some random failures.
    def transform[A](schema: Schema.Sequence[A]): SequenceTransform[A] =
      Schema.Transform[Chunk[A], List[A]](schema, chunk => Right(chunk.toList), list => Right(Chunk.fromIterable(list)))
    anySequence.map(schema => transform(schema))
  }

  type RecordTransform[A <: Product] = Schema.Transform[Map[String, _], A]

  val anyRecordTransform: Gen[Random with Sized, RecordTransform[_]] = {
    // TODO: Dynamically generate a case class.
    def transform(schema: Schema.Record): RecordTransform[_] =
      Schema.Transform[Map[String, _], Any](schema, _ => Left("Not implemented."), _ => Left("Not implemented."))
    anyRecord.map(schema => transform(schema))
  }

  type EnumerationTransform[A] = Schema.Transform[Map[String, _], A]

  val anyEnumerationTransform: Gen[Random with Sized, EnumerationTransform[_]] = {
    // TODO: Dynamically generate a sealed trait and case/value classes.
    def transform(schema: Schema.Enumeration): EnumerationTransform[_] =
      Schema.Transform[Map[String, _], Any](schema, _ => Left("Not implemented."), _ => Left("Not implemented."))
    anyEnumeration.map(schema => transform(schema))
  }

  val anyTransform: Gen[Random with Sized, Schema.Transform[_, _]] = Gen.oneOf(
    anySequenceTransform,
    anyRecordTransform,
    anyEnumerationTransform
  )

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
