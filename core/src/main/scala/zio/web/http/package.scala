package zio.web

package object http extends HttpProtocolModule {
  val defaultProtocol: codec.Codec = codec.json.JsonCodec

  val allProtocols: Map[String, codec.Codec] = Map("application/json" -> codec.json.JsonCodec)
}
