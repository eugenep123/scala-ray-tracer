package chapters

object Settings {

  def width: Int = 1024 * 2
  def height: Int = 768 * 2

  def folder: String = {
    val homeDir = System.getProperty("user.home")
    s"$homeDir/dev/other/output"
  }
}
