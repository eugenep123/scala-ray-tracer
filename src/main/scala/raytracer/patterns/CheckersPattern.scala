package raytracer.patterns

import math.floor
import raytracer.math.{Matrix, Point3D}
import raytracer.Color

case class CheckersPattern(a: Color, b: Color, transform: Matrix) extends Pattern {
  override def patternAt(point: Point3D): Color = {
    val d = floor(point.x) + floor(point.y) + floor(point.z)
    if (d.toInt % 2 == 0) a
    else b
  }

  override type PatternType = CheckersPattern
  override def setTransform(t: Matrix): CheckersPattern = this.copy(transform = t)
}

object CheckersPattern {
  def apply(
    a: Color,
    b: Color,
    transform: Matrix = Matrix.identity): CheckersPattern =
    new CheckersPattern(a, b, transform)
}
