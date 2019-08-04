package chapters

import raytracer.Operations._
import raytracer._
import Shapes._
import raytracer.shapes.Shape

object Chapter9 extends RenderApp {
  //  val width = 1920
  //  val height = 1080
  override def width: Int = super.width
  override def height: Int = super.height

  override def objects: Seq[Shape] = {
    val floor = plane()
    val middle = sphere(
      translation(-0.5, 1, 0.5),
      Material(
        Color(0.1, 1, 0.5),
        diffuse = 0.7,
        specular = 0.3
      )
    )

    val right = sphere(
      translation(1.5, 0.5, -0.5) * scaling(0.5, 0.5, 0.5),
      Material(
        Color(0.5, 1, 0.1),
        diffuse = 0.7,
        specular = 0.3
      )
    )

    val left = sphere(
      translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33),
      Material(
        Color(1, 0.8, 0.1),
        diffuse = 0.7,
        specular = 0.3
      )
    )
    Seq(floor, left, middle, right)
  }

}
