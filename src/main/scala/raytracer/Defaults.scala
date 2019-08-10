package raytracer

import math._
import Operations._
import shapes._

object Defaults {



  val width = 400
  val height = 400
  val camera: Camera = new Camera(
    width,
    height,
    π / 3,
    viewTransform(
      from = Point3D(0, 1.5, -5),
      to = Point3D(0, 1, 0),
      up = Vector3D(0, 1, 0))
  )

  def world: World = {
    val light = PointLight(Point3D(-10, 10, -10), Color(1, 1, 1))
    val s1 = Shapes.sphere(
      material = Material(
      Color(0.8, 1.0, 0.6),
      diffuse = 0.7,
      specular = 0.2
    ))
    val s2 = Shapes.sphere(Operations.scaling(0.5, 0.5, 0.5))
    val objects = Seq(s1, s2)
    new World(objects, Seq(light))
  }

}
