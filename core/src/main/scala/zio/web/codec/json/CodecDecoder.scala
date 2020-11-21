package zio.web.codec.json

import java.time.DayOfWeek

import zio.json.JsonDecoder
import zio.json.internal.RetractReader
import zio.schema.StandardType

object CodecDecoder {

  final def primitiveDecoder[A](standardType: StandardType[A]): JsonDecoder[A] =
    standardType match {
      case StandardType.UnitType           => unitDecoder
      case StandardType.StringType         => JsonDecoder[String]
      case StandardType.BoolType           => JsonDecoder[Boolean]
      case StandardType.ShortType          => JsonDecoder[Short]
      case StandardType.IntType            => JsonDecoder[Int]
      case StandardType.LongType           => JsonDecoder[Long]
      case StandardType.FloatType          => JsonDecoder[Float]
      case StandardType.DoubleType         => JsonDecoder[Double]
      case StandardType.ByteType           => JsonDecoder[Byte]
      case StandardType.CharType           => JsonDecoder[Char]
      case StandardType.DayOfWeekType      => JsonDecoder[DayOfWeek]
      case StandardType.DurationType       => JsonDecoder[java.time.Duration]
      case StandardType.InstantType        => JsonDecoder[java.time.Instant]
      case StandardType.LocalDateType      => JsonDecoder[java.time.LocalDate]
      case StandardType.LocalDateTimeType  => JsonDecoder[java.time.LocalDateTime]
      case StandardType.LocalTimeType      => JsonDecoder[java.time.LocalTime]
      case StandardType.MonthType          => JsonDecoder[java.time.Month]
      case StandardType.MonthDayType       => JsonDecoder[java.time.MonthDay]
      case StandardType.OffsetDateTimeType => JsonDecoder[java.time.OffsetDateTime]
      case StandardType.OffsetTimeType     => JsonDecoder[java.time.OffsetTime]
      case StandardType.PeriodType         => JsonDecoder[java.time.Period]
      case StandardType.YearType           => JsonDecoder[java.time.Year]
      case StandardType.YearMonthType      => JsonDecoder[java.time.YearMonth]
      case StandardType.ZonedDateTimeType  => JsonDecoder[java.time.ZonedDateTime]
      case StandardType.ZoneIdType         => JsonDecoder[java.time.ZoneId]
      case StandardType.ZoneOffsetType     => JsonDecoder[java.time.ZoneOffset]
    }

  private val unitDecoder: JsonDecoder[Unit] =
    (_: List[JsonDecoder.JsonError], _: RetractReader) => ()

}
