package chapters

import raytracer._
import shapes._
import math._
import Operations._

object Chapter7 extends RenderApp {
  import Shapes._

  val floor = sphere(
    scaling(10, 0.01, 10),
    Material(
      color = Color(1, 0.9, 0.9),
      specular = 0
    )
  )

  val leftWall = sphere(
    translation(0, 0, 5) *
      rotationY(-π/4) * rotationX(π/2) *
      scaling(10, 0.01, 10),
    floor.material
  )

  val rightWall = sphere(
    translation(0, 0, 5) *
      rotationY(π/4) * rotationX(π/2) *
      scaling(10, 0.01, 10),
    floor.material
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

  override def objects: Seq[Shape] = Seq(floor, leftWall, rightWall, left, middle, right)
}

// Normal:              Time: 108060.952016 ms
// Par:                 Time:  63115.292707 ms
// with base shape:     Time:   9208.211067 ms
// With base shape par: Time:   5314.858382 ms

