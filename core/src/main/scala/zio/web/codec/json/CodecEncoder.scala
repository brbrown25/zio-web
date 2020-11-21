package zio.web.codec.json

import zio.json.JsonEncoder
import zio.json.internal.Write
import zio.schema.StandardType

object CodecEncoder {
  final def primitiveEncoder[A](standardType: StandardType[A]): JsonEncoder[A] =
    standardType match {
      case StandardType.UnitType =>
        new JsonEncoder[A] {
          override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = ()
        }
      case StandardType.StringType         => JsonEncoder[String]
      case StandardType.BoolType           => JsonEncoder[Boolean]
      case StandardType.ShortType          => JsonEncoder[Short]
      case StandardType.IntType            => JsonEncoder[Int]
      case StandardType.LongType           => JsonEncoder[Long]
      case StandardType.FloatType          => JsonEncoder[Float]
      case StandardType.DoubleType         => JsonEncoder[Double]
      case StandardType.ByteType           => JsonEncoder[Byte]
      case StandardType.CharType           => JsonEncoder[Char]
      case StandardType.DayOfWeekType      => JsonEncoder[java.time.DayOfWeek]
      case StandardType.DurationType       => JsonEncoder[java.time.Duration]
      case StandardType.InstantType        => JsonEncoder[java.time.Instant]
      case StandardType.LocalDateType      => JsonEncoder[java.time.LocalDate]
      case StandardType.LocalDateTimeType  => JsonEncoder[java.time.LocalDateTime]
      case StandardType.LocalTimeType      => JsonEncoder[java.time.LocalTime]
      case StandardType.MonthType          => JsonEncoder[java.time.Month]
      case StandardType.MonthDayType       => JsonEncoder[java.time.MonthDay]
      case StandardType.OffsetDateTimeType => JsonEncoder[java.time.OffsetDateTime]
      case StandardType.OffsetTimeType     => JsonEncoder[java.time.OffsetTime]
      case StandardType.PeriodType         => JsonEncoder[java.time.Period]
      case StandardType.YearType           => JsonEncoder[java.time.Year]
      case StandardType.YearMonthType      => JsonEncoder[java.time.YearMonth]
      case StandardType.ZonedDateTimeType  => JsonEncoder[java.time.ZonedDateTime]
      case StandardType.ZoneIdType         => JsonEncoder[java.time.ZoneId]
      case StandardType.ZoneOffsetType     => JsonEncoder[java.time.ZoneOffset]
    }
}
