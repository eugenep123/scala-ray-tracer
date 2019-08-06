package chapters

import raytracer.Color
import raytracer.math.π
import raytracer.shapes.{Cylinder, Group, Shape, Sphere}

object Hexagon {

  def corner: Sphere = {
    Shape().scale(0.25).translate(0, 0, -1).sphere
  }

  def edge: Cylinder = {
    Shape()
      .scale(0.25, 1, 0.25)
      .rotateZ(-π/2)
      .rotateY(-π/6)
      .translate(0, 0, -1)
      .cylinder(0, 1)
  }

  def side(n: Int): Group = {
    Shape()
      .rotateY(n * π/3)
      .group(corner, edge)
  }

  def build(): Group = {
    Shape()
      .setColor(Color.Red)
      .groupFill(6)(side)
  }
}
