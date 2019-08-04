package raytracer.files

import raytracer.Shape
import raytracer.waveform.ObjFileParser

trait ResourceLoader {
  def loadObject(filename: String): Shape
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {
  implicit def default: ResourceLoader = new ResourceLoader {
    override def loadObject(filename: String): Shape = {
      //TODO: inject path via implicit
      val name = s"/examples/triangles/$filename"
      val content = FileIO.getResourceString(name)
      ObjFileParser.parseGroup(content)
    }
  }
}