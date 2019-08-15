package chapters.experiments

import chapters.RenderApp
import raytracer.math.Transforms.ViewTransform
import raytracer.{Camera, Color, PointLight}
import raytracer.patterns.CheckersPattern
import raytracer.shapes.Shape
import raytracer.math._

object PovExample extends RenderApp {

  override def objects: Seq[Shape] = {

    val plane = Shape()
      .setPattern(CheckersPattern(Color.Black, Color.White))
      .setReflective(0.3)
      .plane

    val sphere1 = Shape()
      .scale(3)
      .translate(-7, 3, -2)
      .setColor(1, 0, 0)
      .setReflective(0.4)
      .sphere

    val sphere2 = Shape()
      .transform(_.scale(3).translate(0, 3, -1))
      .setColor(0, 1, 0)
      .setReflective(0.6)
      .sphere

    val sphere3 = Shape()
      .transform(_.scale(3).translate(6.5, 3, 0))
      .setColor(0, 0, 1)
      .setReflective(0.8)
      .sphere

    Seq(plane, sphere1, sphere2, sphere3)
  }

  override def lights: Seq[PointLight] = Seq(
    PointLight(Point3D(0, 20, 20), Color(1, 1, 1))
  )

  override def camera: Camera = Camera(width, height, π/3,
    ViewTransform(
      Point3D(2, 4, 20),
      Point3D(0, 0, 0),
      Vector3D(0, 1, 0))
  )

}