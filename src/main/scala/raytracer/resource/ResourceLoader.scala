package raytracer.resource

import raytracer.resource.waveform.ObjFileParser
import raytracer.shapes.Group

trait ResourceLoader {
  def loadObject(filename: String): Group
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {

  // Loads objects from the resource folder: 'objects'
  implicit def default: ResourceLoader = (filename: String) => {
    val resourceName = s"/objects/$filename"
    val content = getResourceString(resourceName)
    ObjFileParser.parseGroup(content)
  }
}