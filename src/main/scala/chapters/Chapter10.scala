package chapters

import raytracer._
import patterns._
import shapes._
import Color._
import Shapes._
import math._
import Operations._

object Chapter10 extends RenderApp {
  override def objects: Seq[Shape] = {


    val floor = plane(
      rotationY(PI/2),
      Material().setPattern(StripePattern(Red, Blue, scaling(2, 2, 2)))
    )

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
      ).setPattern(CheckersPattern(Blue, White))
    )

    val left = sphere(
      translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33),
      Material(
        Color(1, 0.8, 0.1),
        diffuse = 0.7,
        specular = 0.3
      ).setPattern(GradientPattern(Red, Blue))
    )

    Seq(floor, left, middle, right)
  }
}
