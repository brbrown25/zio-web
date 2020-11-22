package zio.schema

import zio.random.Random
import zio.test.{ Gen, Sized }

object StandardTypeGen {

  val anyStandardType: Gen[Random, StandardType[_]] = Gen.oneOf(
    Gen.const(StandardType.UnitType),
    Gen.const(StandardType.StringType),
    Gen.const(StandardType.BoolType),
    Gen.const(StandardType.ShortType),
    Gen.const(StandardType.IntType),
    Gen.const(StandardType.LongType),
    Gen.const(StandardType.FloatType),
    Gen.const(StandardType.DoubleType),
    Gen.const(StandardType.ByteType),
    Gen.const(StandardType.CharType),
    Gen.const(StandardType.DayOfWeekType),
    Gen.const(StandardType.DurationType),
    Gen.const(StandardType.InstantType),
    Gen.const(StandardType.LocalDateType),
    Gen.const(StandardType.LocalDateTimeType),
    Gen.const(StandardType.LocalTimeType),
    Gen.const(StandardType.MonthType),
    Gen.const(StandardType.MonthDayType),
    Gen.const(StandardType.OffsetDateTimeType),
    Gen.const(StandardType.OffsetTimeType),
    Gen.const(StandardType.PeriodType),
    Gen.const(StandardType.YearType),
    Gen.const(StandardType.YearMonthType),
    Gen.const(StandardType.ZonedDateTimeType),
    Gen.const(StandardType.ZoneIdType),
    Gen.const(StandardType.ZoneOffsetType)
  )

  type StandardTypeAndValue[A] = (StandardType[A], A)

  val anyStandardTypeAndValue: Gen[Random with Sized, StandardTypeAndValue[_]] = anyStandardType.flatMap {
    case typ @ StandardType.UnitType           => Gen.const(typ -> ())
    case typ @ StandardType.StringType         => Gen.const(typ).zip(Gen.anyString)
    case typ @ StandardType.BoolType           => Gen.const(typ).zip(Gen.boolean)
    case typ @ StandardType.ShortType          => Gen.const(typ).zip(Gen.anyShort)
    case typ @ StandardType.IntType            => Gen.const(typ).zip(Gen.anyInt)
    case typ @ StandardType.LongType           => Gen.const(typ).zip(Gen.anyLong)
    case typ @ StandardType.FloatType          => Gen.const(typ).zip(Gen.anyFloat)
    case typ @ StandardType.DoubleType         => Gen.const(typ).zip(Gen.anyDouble)
    case typ @ StandardType.ByteType           => Gen.const(typ).zip(Gen.anyByte)
    case typ @ StandardType.CharType           => Gen.const(typ).zip(Gen.anyChar)
    case typ @ StandardType.DayOfWeekType      => Gen.const(typ).zip(JavaTimeGen.anyDayOfWeek)
    case typ @ StandardType.DurationType       => Gen.const(typ).zip(JavaTimeGen.anyDuration)
    case typ @ StandardType.InstantType        => Gen.const(typ).zip(JavaTimeGen.anyInstant)
    case typ @ StandardType.LocalDateType      => Gen.const(typ).zip(JavaTimeGen.anyLocalDate)
    case typ @ StandardType.LocalDateTimeType  => Gen.const(typ).zip(JavaTimeGen.anyLocalDateTime)
    case typ @ StandardType.LocalTimeType      => Gen.const(typ).zip(JavaTimeGen.anyLocalTime)
    case typ @ StandardType.MonthType          => Gen.const(typ).zip(JavaTimeGen.anyMonth)
    case typ @ StandardType.MonthDayType       => Gen.const(typ).zip(JavaTimeGen.anyMonthDay)
    case typ @ StandardType.OffsetDateTimeType => Gen.const(typ).zip(JavaTimeGen.anyOffsetDateTime)
    case typ @ StandardType.OffsetTimeType     => Gen.const(typ).zip(JavaTimeGen.anyOffsetTime)
    case typ @ StandardType.PeriodType         => Gen.const(typ).zip(JavaTimeGen.anyPeriod)
    case typ @ StandardType.YearType           => Gen.const(typ).zip(JavaTimeGen.anyYear)
    case typ @ StandardType.YearMonthType      => Gen.const(typ).zip(JavaTimeGen.anyYearMonth)
    case typ @ StandardType.ZonedDateTimeType  => Gen.const(typ).zip(JavaTimeGen.anyZonedDateTime)
    case typ @ StandardType.ZoneIdType         => Gen.const(typ).zip(JavaTimeGen.anyZoneId)
    case typ @ StandardType.ZoneOffsetType     => Gen.const(typ).zip(JavaTimeGen.anyZoneOffset)
  }
}
