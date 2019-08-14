package raytracer.resource

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import raytracer.{Canvas, Color}
import raytracer.resource.ppm.PPMParser
import raytracer.resource.waveform.{ObjBuilder, ObjParser}
import raytracer.shapes.Group

trait ResourceLoader {
  def loadImage(file: String): Canvas

  def loadObject(filename: String): Group
  //    def readMaterial(filename: String): Group
}
object ResourceLoader {

  implicit val default = new ResourceLoader {

    override def loadObject(filename: String): Group = {
      val resourceName = s"/objects/$filename"
      val content = getResourceString(resourceName)
      val parser = new ObjParser(ObjBuilder)
      //    val normalized = Point3D.normalize(vertices)
      parser.parse(content).toGroup()
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