package raytracer.resource

import raytracer.resource.waveform.ObjFileParser
import raytracer.shapes.Group

trait ResourceLoader {
  def loadObject(filename: String): Group
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {
  implicit def default: ResourceLoader = new ResourceLoader {
    override def loadObject(filename: String): Group = {
      //TODO: inject path via implicit
//      val name = s"/examples/triangles/$filename"
      val name = s"/examples/triangles/cube.obj"
      val content = getResourceString(name)
      ObjFileParser.parseGroup(content)
    }
  }
}