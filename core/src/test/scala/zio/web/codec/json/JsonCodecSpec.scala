package zio.web.codec.json

import zio.Chunk
import zio.duration._
import zio.schema.{ Schema, SchemaGen, StandardType }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.environment.TestEnvironment
import zio.test.{ testM, _ }

//TODO encode and decode specs
object JsonCodecSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] =
    suite("JsonCodecSpec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite,
      decoderEncoderSuite
    ) @@ timeout(10.seconds)

  // TODO: Add tests for the transducer contract.

  private val encoderSuite = suite("encoder") {
    suite("primitive")(
      testM("unit") {
        assertEncodesUnit
      },
      suite("string")(
        testM("example") {
          assertEncodesString("hello")
        },
        testM("any") {
          checkM(Gen.anyString)(assertEncodesString)
        }
      )
    )
  }

  private val decoderSuite = suite("decoder") {
    suite("primitive")(
      testM("unit") {
        assertDecodesUnit
      },
      suite("string")(
        testM("example") {
          assertDecodesString("hello")
        },
        testM("any") {
          checkM(Gen.anyString)(assertDecodesString)
        }
      )
    )
  }

  private val encoderDecoderSuite = suite("encoder -> decoder")(
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    testM("optional") {
      checkM(SchemaGen.anyOptionalAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    testM("any") {
      checkM(SchemaGen.anySchemaAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    }
  )

  private val decoderEncoderSuite = suite("decoder -> encoder")(
    )

  private def assertEncodesUnit = {
    val schema = Schema.Primitive(StandardType.UnitType)
    assertEncodes(schema, (), Chunk.empty)
  }

  private def assertEncodesString(value: String) = {
    val schema = Schema.Primitive(StandardType.StringType)
    assertEncodes(schema, value, Chunk.fromIterable(stringify(value)))
  }

  private def assertEncodes[A](schema: Schema[A], value: A, chunk: Chunk[Char]) = {
    val stream = JsonCodec.encoder(schema).encodeJsonStream(value, None).runCollect
    assertM(stream)(equalTo(chunk))
  }

  private def assertDecodesUnit = {
    val schema = Schema.Primitive(StandardType.UnitType)
    assertDecodes(schema, (), Chunk.empty)
  }

  private def assertDecodesString(value: String) = {
    val schema = Schema.Primitive(StandardType.StringType)
    assertDecodes(schema, value, Chunk.fromIterable(stringify(value)))
  }

  private def assertDecodes[A](schema: Schema[A], value: A, chunk: Chunk[Char]) = {
    val stream = ZStream.fromChunk(chunk)
    val result = JsonCodec.decoder(schema).decodeJsonStream(stream)
    assertM(result)(equalTo(value))
  }

  private def assertEncodesThenDecodes[A](schema: Schema[A], value: A) = {
    val stream = JsonCodec.encoder(schema).encodeJsonStream(value, None)
    val result = JsonCodec.decoder(schema).decodeJsonStream(stream)
    assertM(result)(equalTo(value))
  }

  private def stringify(s: String): String = s""""${escape(s)}""""

  private def escape(s: String): String = {
    val builder = new StringBuilder
    s.map {
      case '"'          => builder.append("\\\"")
      case '\\'         => builder.append("\\\\")
      case '\b'         => builder.append("\\b")
      case '\f'         => builder.append("\\f")
      case '\n'         => builder.append("\\n")
      case '\r'         => builder.append("\\r")
      case '\t'         => builder.append("\\t")
      case c if c < ' ' => builder.append("\\u%04x".format(c.toInt))
      case c            => builder.append(c)
    }
    builder.toString
  }
}
