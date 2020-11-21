package zio.web.codec

import zio.blocking.Blocking
import zio.test.{ DefaultRunnableSpec, Spec, TestFailure, TestSuccess }

//TODO encode and decode specs
class JsonCodecSpec extends DefaultRunnableSpec {
  def spec: Spec[Blocking, TestFailure[Throwable], TestSuccess] = ???
}
