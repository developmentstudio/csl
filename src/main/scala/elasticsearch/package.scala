import wabisabi.Client

package object elasticsearch {

  type Query = String

  private val hostname = Config.setting("settings.elasticsearch.hostname")
  private val port = Config.setting("settings.elasticsearch.port")

  val client = new Client(s"http://$hostname:$port")

}
