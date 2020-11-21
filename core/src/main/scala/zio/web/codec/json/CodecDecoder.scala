package zio.web.codec.json

import java.time.DayOfWeek

import zio.ZIO
import zio.blocking.Blocking
import zio.json.{ JsonDecoder, JsonStreamDelimiter }
import zio.schema.StandardType
import zio.stream.ZTransducer

object CodecDecoder {
  final def primitiveDecoder[A](standardType: StandardType[A]): ZTransducer[Blocking, String, Byte, A] =
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
      dec.decodeJsonTransducer(JsonStreamDelimiter.Newline)).mapError(_.getMessage)

}
