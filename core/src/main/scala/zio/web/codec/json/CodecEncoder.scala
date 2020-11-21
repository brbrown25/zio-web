package zio.web.codec.json

import zio.{ Chunk, ZIO }
import zio.json.JsonEncoder
import zio.stream.ZTransducer
import zio.schema.StandardType

object CodecEncoder {
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

  final def primitiveEncoder[A](standardType: StandardType[A]): ZTransducer[Any, Nothing, A, Byte] =
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

}
