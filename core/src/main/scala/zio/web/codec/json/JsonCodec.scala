package zio.web.codec.json

import zio.json.{ JsonDecoder, JsonEncoder }
import zio.schema._
import zio.web.codec.Codec

// TODO: Should this be a class that takes a character encoding parameter?
object JsonCodec extends Codec {

  override def encoder[A](schema: Schema[A]): JsonEncoder[A] = schema match {
    case Schema.Record(_)               => ???
    case Schema.Sequence(_)             => ???
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Primitive(standardType) => CodecEncoder.primitiveEncoder(standardType)
    case Schema.Tuple(_, _)             => ???
    case Schema.Optional(o)             => encoder(o.asInstanceOf[Schema[A]])
  }

  override def decoder[A](schema: Schema[A]): JsonDecoder[A] = schema match {
    case Schema.Record(_)               => ???
    case Schema.Sequence(_)             => ???
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Primitive(standardType) => CodecDecoder.primitiveDecoder(standardType)
    case Schema.Tuple(_, _)             => ???
    case Schema.Optional(_)             => ???
  }
}
