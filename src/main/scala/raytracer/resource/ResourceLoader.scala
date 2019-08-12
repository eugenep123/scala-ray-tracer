package raytracer.resource

import raytracer.Canvas
import raytracer.resource.ppm.PPMParser
import raytracer.resource.waveform.ObjFileParser
import raytracer.shapes.Group

trait ResourceLoader {
  def loadImage(file: String): Canvas

  def loadObject(filename: String): Group
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {

  val default = new ResourceLoader {

    override def loadObject(filename: String): Group = {
      val resourceName = s"/objects/$filename"
      val content = getResourceString(resourceName)
      ObjFileParser.parseGroup(content)
    }

    override def loadImage(filename: String): Canvas = {
      val resourceName = s"/textures/$filename"
      val content = getResourceString(resourceName)
      val canvas = PPMParser.parse(content)
      canvas.getOrElse(throw new RuntimeException(s"Error loading image: '$filename'"))
    }
  }




}