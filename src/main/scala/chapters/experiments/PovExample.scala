package chapters.experiments

import chapters.{RenderApp, Settings}
import raytracer.Operation._
import raytracer._
import raytracer.patterns._
import raytracer.shapes._

object PovExample extends RenderApp {


  override def objects: Seq[Shape] = {

    val plane = Plane()
      .setPattern(CheckersPattern(Color.Black, Color.White))
      .setReflective(0.3)

    val sphere1 = Sphere()
      .builtTransform(_.scale(3).translate(-7, 3, -2))
      .setColor(1, 0, 0)
      .setReflective(0.4)

    val sphere2 = Sphere()
      .builtTransform(_.scale(3).translate(0, 3, -1))
      .setColor(0, 1, 0)
      .setReflective(0.6)

    val sphere3 = Sphere()
      .builtTransform(_.scale(3).translate(6.5, 3, 0))
      .setColor(0, 0, 1)
      .setReflective(0.8)

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