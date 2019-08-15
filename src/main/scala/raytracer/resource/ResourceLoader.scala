package raytracer.resource

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import performance.obj.ObjResult
import raytracer.{Canvas, Color}
import raytracer.resource.ppm.PPMParser
import raytracer.resource.waveform.{ObjBuilder, ObjParser}
import raytracer.shapes.Group
import sun.jvm.hotspot.utilities.ObjectReader

trait ResourceLoader {
  def loadImage(file: String): Canvas

  def loadObject(filename: String, normalize: Boolean): Group
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {

  implicit val default = new ResourceLoader {

    private var objectCache = Map.empty[String, ObjResult]

    override def loadObject(filename: String, normalize: Boolean): Group = {
      val resourceName = s"/objects/$filename"
      val result = objectCache.get(filename) match {
        case Some(result) =>
          println(s"Using cached obj file: $filename")
          result
        case None =>
          println(s"Reading obj file: $filename")
          val content = getResourceString(resourceName)
          val parser = new ObjParser(new ObjBuilder)
          val r = parser.parse(content)
          val r2 = if (normalize) r.normalize else r
          objectCache += (filename -> r2)
          r2
      }
      result.toGroup()
    }

    override def loadImage(filename: String): Canvas = {
      if (filename.endsWith("ppm")) loadPPMImage(filename)
      else loadJpg(filename)
    }

    def loadJpg(filename: String) = {
      val path = new File(s"../images/skyboxes/LancellottiChapel/$filename").getAbsoluteFile
      val image = ImageIO.read(path)
      toCanvas(image)
    }

    def loadPPMImage(filename: String): Canvas = {
      println(s"Reading image: $filename...")
      val resourceName = s"/textures/$filename"
      val content = getResourceString(resourceName)
      val canvas = PPMParser.parse(content)
      canvas.getOrElse(throw new RuntimeException(s"Error loading image: '$filename'"))
    }
  }

  def toCanvas(image: BufferedImage): Canvas = {
    val canvas = Canvas(image.getWidth(), image.getHeight())
    for {
      x <- 0 until canvas.width
      y <- 0 until canvas.height
    } {
      val rgb = image.getRGB(x, y)
      val c = Color.fromRgb(rgb)
      canvas.writePixel(x, y, c)
    }
    canvas
  }



}