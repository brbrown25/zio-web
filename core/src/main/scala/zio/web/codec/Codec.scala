package zio.web.codec

import zio.json.{ JsonDecoder, JsonEncoder }
import zio.schema.Schema

trait Codec {
  def encoder[A](schema: Schema[A]): JsonEncoder[A]
  def decoder[A](schema: Schema[A]): JsonDecoder[A]
}
