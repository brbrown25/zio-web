package zio.web.codec

import zio.Chunk
import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment
import zio.web.codec.json.JsonCodec
import zio.schema.{ Schema, StandardType }
import zio.web.schema.SchemaGen

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
        JsonCodec.encoder(Schema.Primitive(StandardType.UnitType)).push.use { push =>
          for {
            part1 <- push(Some(Chunk(())))
            part2 <- push(None)
          } yield assert(part1 ++ part2)(equalTo(Chunk.empty))
        }
      }
    }
  }

  private val decoderSuite = suite("decoder") {
    suite("primitive") {
      testM("unit") {
        JsonCodec.decoder(Schema.Primitive(StandardType.UnitType)).push.use { push =>
          for {
            part <- push(None)
          } yield assert(part)(equalTo(Chunk(())))
        }
      }
    }
  }

  private val encoderDecoderSuite = suite("encoder -> decoder") {
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) =>
          // TODO: Generate more than one value.
          val data = Chunk.single(value)
          (JsonCodec.encoder(schema) >>> JsonCodec.decoder(schema)).push.use { push =>
            for {
              _     <- zio.console.putStrLn(s"value: ${value.toString}")
              _     <- zio.console.putStrLn(s"data: ${data.toString}")
              part1 <- push(Some(data))
              part2 <- push(None)
              _     <- zio.console.putStrLn(s"result: ${(part1 ++ part2).toString}")
            } yield assert(part1 ++ part2)(equalTo(data))
          }
      }
    }
  }

  private val decoderEncoderSuite = suite("decoder -> encoder")(
    )
}
