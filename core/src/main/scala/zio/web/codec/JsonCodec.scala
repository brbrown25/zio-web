package zio.web.codec

import java.time.DayOfWeek

import zio.blocking.Blocking
import zio.json.JsonCodec.apply
import zio.json.{ JsonDecoder, JsonEncoder }
import zio.stream.ZTransducer
import zio.web.schema._
import zio.{ Chunk, ZIO }

// TODO: Should this be a class that takes a character encoding parameter?
object JsonCodec extends Codec {

  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] = schema match {
    case Schema.Primitive(standardType) => primitiveEncoder(standardType)
    case Schema.Record(_)               => ???
    case Schema.Sequence(elem)          => encoder(elem)
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Tuple(l, r)             => encoder(l) >>> encoder(r)
    case Schema.Optional(c)             => encoder(c)
  }

  private val unitEncoder: ZTransducer[Any, Nothing, Unit, Nothing] =
    ZTransducer.fromPush(_ => ZIO.succeed(Chunk.empty))

  private def standardEncoder[A](implicit enc: JsonEncoder[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer
      .fromFunction[A, Chunk[Byte]] { s =>
        charSequenceToByteChunk(enc.encodeJson(s, None))
      }
      .mapChunks(_.flatten)

  private def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
    val bytes: Seq[Byte] = for (i <- 0 until chars.length) yield chars.charAt(i).toByte
    Chunk.fromIterable(bytes)
  }

  private def primitiveEncoder[A](standardType: StandardType[A]): ZTransducer[Any, Nothing, A, Byte] =
    standardType match {
      case StandardType.UnitType           => unitEncoder
      case StandardType.StringType         => standardEncoder[String]
      case StandardType.BoolType           => standardEncoder[Boolean]
      case StandardType.ShortType          => standardEncoder[Short]
      case StandardType.IntType            => standardEncoder[Int]
      case StandardType.LongType           => standardEncoder[Long]
      case StandardType.FloatType          => standardEncoder[Float]
      case StandardType.DoubleType         => standardEncoder[Double]
      case StandardType.ByteType           => standardEncoder[Byte]
      case StandardType.CharType           => standardEncoder[Char]
      case StandardType.DayOfWeekType      => standardEncoder[java.time.DayOfWeek]
      case StandardType.DurationType       => standardEncoder[java.time.Duration]
      case StandardType.InstantType        => standardEncoder[java.time.Instant]
      case StandardType.LocalDateType      => standardEncoder[java.time.LocalDate]
      case StandardType.LocalDateTimeType  => standardEncoder[java.time.LocalDateTime]
      case StandardType.LocalTimeType      => standardEncoder[java.time.LocalTime]
      case StandardType.MonthType          => standardEncoder[java.time.Month]
      case StandardType.MonthDayType       => standardEncoder[java.time.MonthDay]
      case StandardType.OffsetDateTimeType => standardEncoder[java.time.OffsetDateTime]
      case StandardType.OffsetTimeType     => standardEncoder[java.time.OffsetTime]
      case StandardType.PeriodType         => standardEncoder[java.time.Period]
      case StandardType.YearType           => standardEncoder[java.time.Year]
      case StandardType.YearMonthType      => standardEncoder[java.time.YearMonth]
      case StandardType.ZonedDateTimeType  => standardEncoder[java.time.ZonedDateTime]
      case StandardType.ZoneIdType         => standardEncoder[java.time.ZoneId]
      case StandardType.ZoneOffsetType     => standardEncoder[java.time.ZoneOffset]
    }

  override def decoder[A](schema: Schema[A]): ZTransducer[Blocking, String, Byte, A] = schema match {
    case Schema.Primitive(standardType) => primitiveDecoder(standardType)
    case Schema.Record(_)               => ???
    case Schema.Sequence(_)             => ???
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Tuple(_, _)             => ???
    case Schema.Optional(_)             => ???
  }

  private def primitiveDecoder[A](standardType: StandardType[A]): ZTransducer[Blocking, String, Byte, A] =
    standardType match {
      case StandardType.UnitType           => unitDecoder
      case StandardType.StringType         => standardDecoder[String]
      case StandardType.BoolType           => standardDecoder[Boolean]
      case StandardType.ShortType          => standardDecoder[Short]
      case StandardType.IntType            => standardDecoder[Int]
      case StandardType.LongType           => standardDecoder[Long]
      case StandardType.FloatType          => standardDecoder[Float]
      case StandardType.DoubleType         => standardDecoder[Double]
      case StandardType.ByteType           => standardDecoder[Byte]
      case StandardType.CharType           => standardDecoder[Char]
      case StandardType.DayOfWeekType      => standardDecoder[DayOfWeek]
      case StandardType.DurationType       => standardDecoder[java.time.Duration]
      case StandardType.InstantType        => standardDecoder[java.time.Instant]
      case StandardType.LocalDateType      => standardDecoder[java.time.LocalDate]
      case StandardType.LocalDateTimeType  => standardDecoder[java.time.LocalDateTime]
      case StandardType.LocalTimeType      => standardDecoder[java.time.LocalTime]
      case StandardType.MonthType          => standardDecoder[java.time.Month]
      case StandardType.MonthDayType       => standardDecoder[java.time.MonthDay]
      case StandardType.OffsetDateTimeType => standardDecoder[java.time.OffsetDateTime]
      case StandardType.OffsetTimeType     => standardDecoder[java.time.OffsetTime]
      case StandardType.PeriodType         => standardDecoder[java.time.Period]
      case StandardType.YearType           => standardDecoder[java.time.Year]
      case StandardType.YearMonthType      => standardDecoder[java.time.YearMonth]
      case StandardType.ZonedDateTimeType  => standardDecoder[java.time.ZonedDateTime]
      case StandardType.ZoneIdType         => standardDecoder[java.time.ZoneId]
      case StandardType.ZoneOffsetType     => standardDecoder[java.time.ZoneOffset]
    }

  private val unitDecoder: ZTransducer[Any, Nothing, Byte, Unit] =
    ZTransducer.branchAfter(0)(_ => ZTransducer.fromEffect(ZIO.unit))

  private def standardDecoder[A](implicit dec: JsonDecoder[A]): ZTransducer[Blocking, String, Byte, A] =
    (ZTransducer.utfDecode.mapChunks(_.flatMap(s => s)) >>>
      dec.decodeJsonTransducer()).mapError(_.getMessage)
}
