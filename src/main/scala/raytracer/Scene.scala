package raytracer

import resource.yaml.YamlParser
import org.scalameter.measure

case class Scene(world: World, camera: Camera) {

  def renderTo(canvas: Canvas): Unit = {
    val time = measure {
      camera.render(world)(canvas.writePixel)
    }
    println(s"Render time: $time")
  }

  def saveTo(folder: String): Unit = {
    val canvas = Canvas(camera.width, camera.height)
    renderTo(canvas)
    canvas.save(folder)
  }

  def withDimensions(width: Int, height: Int): Scene =
    Scene(world, camera.copy(width = width, height = height))
}

object Scene {
  def fromResourceYaml(resourceName: String): Scene = {
    val result = YamlParser.readResource(resourceName).get
    println(result)
    result.build
  }
}