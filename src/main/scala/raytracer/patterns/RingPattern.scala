package raytracer.patterns

import raytracer.math.{Matrix, Point3D}
import raytracer.Color
import math.{floor, pow, sqrt}

case class RingPattern(a: Color, b: Color, transform: Matrix) extends Pattern {
  override def patternAt(point: Point3D): Color = {
    val d = floor(sqrt(pow(point.x, 2) + pow(point.z, 2)))
    if (d.toInt % 2 == 0) a
    else b
  }

  override type PatternType = RingPattern
  override def setTransform(t: Matrix): RingPattern = this.copy(transform = t)
}
object RingPattern {
  def apply(
    a: Color,
    b: Color,
    transform: Matrix = Matrix.identity): RingPattern =
    new RingPattern(a, b, transform)
}
