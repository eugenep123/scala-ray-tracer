package raytracer.files

object FileIO {

  def getResourceString(name: String): String = {
    val stream = this.getClass.getResourceAsStream(name)
    scala.io.Source.fromInputStream(stream).getLines.mkString("\n")
  }

}
