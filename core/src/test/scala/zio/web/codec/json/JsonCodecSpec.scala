package zio.web.codec.json

import zio.Chunk
import zio.duration._
import zio.schema.{ Schema, SchemaGen, StandardType }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment.TestEnvironment

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
        val schema = Schema.Primitive(StandardType.UnitType)
        val stream = JsonCodec.encoder(schema).encodeJsonStream((), None).runCollect
        assertM(stream)(equalTo(Chunk.empty))
      }
    )
  }

  private val decoderSuite = suite("decoder") {
    suite("primitive")(
      testM("unit") {
        val schema = Schema.Primitive(StandardType.UnitType)
        val stream = ZStream.empty
        val result = JsonCodec.decoder(schema).decodeJsonStream(stream)
        assertM(result)(isUnit)
      },
      suite("string")(
        testM("example") {
          val value  = "hello"
          val schema = Schema.Primitive(StandardType.StringType)
          val stream = ZStream(stringify(value): _*)
          val result = JsonCodec.decoder(schema).decodeJsonStream(stream)
          assertM(result)(equalTo(value))
        },
        testM("any") {
          checkM(Gen.anyString) { value =>
            val schema = Schema.Primitive(StandardType.StringType)
            val stream = ZStream(stringify(value)).mapConcat(s => s)
            val result = JsonCodec.decoder(schema).decodeJsonStream(stream)
            assertM(result)(equalTo(value))
          }
        }
      )
    )
  }

  private val encoderDecoderSuite = suite("encoder -> decoder")(
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) =>
          val stream = JsonCodec.encoder(schema).encodeJsonStream(value, None)
          val result = JsonCodec.decoder(schema).decodeJsonStream(stream).flatMap { s =>
            zio.console.putStrLn(s"${s.getClass}: ${s.toString}").map(_ => s)
          }
          assertM(result)(equalTo(value))
      }
    }
  )

  private val decoderEncoderSuite = suite("decoder -> encoder")(
    )

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
