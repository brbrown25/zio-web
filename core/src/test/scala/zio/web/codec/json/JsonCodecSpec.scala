package zio.web.codec.json

import zio.Chunk
import zio.schema.{Schema, SchemaGen, StandardType}
import zio.stream.ZStream
import zio.test.Assertion.{equalTo, isUnit, succeeds}
import zio.test._
import zio.test.environment.TestEnvironment

//TODO encode and decode specs
object JsonCodecSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] = suite("JsonCodecSpec")(
    encoderSuite,
    decoderSuite,
    encoderDecoderSuite,
    decoderEncoderSuite
  )

  // TODO: Add tests for the transducer contract.

  private val encoderSuite = suite("encoder") {
    suite("primitive") {
      testM("unit") {
        val schema = Schema.Primitive(StandardType.UnitType)
        val stream = JsonCodec.encoder(schema).encodeJsonStream((), None).runCollect
        assertM(stream)(equalTo(Chunk.empty))
      }
    }
  }

  private val decoderSuite = suite("decoder") {
    suite("primitive") {
      testM("unit") {
        val schema = Schema.Primitive(StandardType.UnitType)
        val stream = ZStream.empty
        val result = JsonCodec.decoder(schema).decodeJsonStream(stream).run
        assertM(result)(succeeds(isUnit))
      }
    }
  }

  private val encoderDecoderSuite = suite("encoder -> decoder") {
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) =>
          val stream = JsonCodec.encoder(schema).encodeJsonStream(value, None)
          val result = JsonCodec.decoder(schema).decodeJsonStream(stream).run
          assertM(result)(succeeds(equalTo(value)))
      }
    }
  }

  private val decoderEncoderSuite = suite("decoder -> encoder")(
    )
}
