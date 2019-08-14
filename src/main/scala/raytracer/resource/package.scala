package raytracer



import scala.io.Source

package object resource {

  def getResourceString(name: String): String = {
    val stream = this.getClass.getResourceAsStream(name)
    scala.io.Source.fromInputStream(stream).getLines.mkString("\n")
  }

  def readFile(filename: String): String = {
    Source.fromFile(filename).getLines.mkString("\n")
  }
}
