package raytracer

import java.awt.Desktop

import resource.yaml.Yaml
import org.scalameter.measure
import java.io.File

case class Scene(world: World, camera: Camera) {

  def withDimensions(width: Int, height: Int): Scene =
    Scene(world, camera.copy(width = width, height = height))

  def renderTo(canvas: Canvas): Unit = {
    val time = measure {
      camera.render(world)(canvas.writePixel)
    }
    println(s"Render time: $time")
  }

  def saveTo(folder: String): File = {
    val canvas = Canvas(camera.width, camera.height)
    renderTo(canvas)
    canvas.save(folder)
  }

  def saveToAndOpen(folder: String): Unit = {
    val file = saveTo(folder)
    Desktop.getDesktop().open(file)
  }


}

object Scene {
  def fromResourceYaml(resourceName: String): Scene = {
    Yaml.readFromResource(resourceName)
  }
}