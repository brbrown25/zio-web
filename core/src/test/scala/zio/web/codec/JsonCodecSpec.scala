package zio.web.codec

import zio.Chunk
import zio.test.Assertion.equalTo
import zio.test._
import zio.test.environment.TestEnvironment
import zio.web.schema.SchemaGen

//TODO encode and decode specs
object JsonCodecSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] = suite("JsonCodecSpec")(
    encoderSuite,
    decoderSuite,
    encoderDecoderSuite,
    decoderEncoderSuite
  )

  private val encoderSuite = suite("encoder")(
    )

  private val decoderSuite = suite("decoder")(
    )

  private val encoderDecoderSuite = suite("encoder -> decoder")(
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) =>
          // TODO: Generate more than one value.
          val data = Chunk.single(value)
          (JsonCodec.encoder(schema) >>> JsonCodec.decoder(schema)).push.use { push =>
            for {
              part1 <- push(Some(data))
              part2 <- push(None)
            } yield assert(part1 ++ part2)(equalTo(data))
          }
      }
    }
  )

  private val decoderEncoderSuite = suite("decoder -> encoder")(
    )
}
