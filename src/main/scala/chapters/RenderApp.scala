package chapters

import raytracer.Operations._
import raytracer._

abstract class RenderApp {

  def width: Int = Settings.width
  def height: Int = Settings.width

  def objects: Seq[Shape]

  def lights = Seq(
    PointLight(Point3D(-10, 10, -10), Color(1, 1, 1))
  )

  def world = World(objects, lights)

  def camera: Camera = {
    Camera(width, height, π/3,
      viewTransform(Point3D(0, 1.5, -5), Point3D(0, 1, 0), Vector3D(0, 1, 0)))
  }

  def scene: Scene = Scene(world, camera)

  def render(canvas: Canvas): Unit = {
    scene.renderTo(canvas)
  }

  def main(args: Array[String]): Unit = {
    scene.saveTo(Settings.folder)
  }
}
