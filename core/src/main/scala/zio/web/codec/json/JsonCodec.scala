package zio.web.codec.json

import zio.blocking.Blocking
import zio.stream.ZTransducer
import zio.web.codec.Codec
import zio.schema._

// TODO: Should this be a class that takes a character encoding parameter?
object JsonCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] = schema match {
    case Schema.Primitive(standardType) => CodecEncoder.primitiveEncoder(standardType)
    case Schema.Record(_)               => ???
    case Schema.Sequence(elem)          => encoder(elem)
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Tuple(l, r)             => encoder(l) >>> encoder(r)
    case Schema.Optional(c)             => encoder(c)
  }

  override def decoder[A](schema: Schema[A]): ZTransducer[Blocking, String, Byte, A] =
    schema match {
      case Schema.Primitive(standardType) => CodecDecoder.primitiveDecoder(standardType)
      case Schema.Record(_)               => ???
      case Schema.Sequence(_)             => ???
      case Schema.Enumeration(_)          => ???
      case Schema.Transform(_, _, _)      => ???
      case Schema.Tuple(_, _)             => ???
      case Schema.Optional(_)             => ???
    }
}
