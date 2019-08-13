package chapters

object Settings {

  def width: Int = 1024
  def height: Int = 768

  def folder: String = {
    val homeDir = System.getProperty("user.home")
    s"$homeDir/dev/other/output"
  }
}
