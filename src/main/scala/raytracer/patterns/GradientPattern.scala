package raytracer
package patterns

import math.floor

case class GradientPattern(a: Color, b: Color, transform: Matrix) extends BasePattern {
  override def colorAt(point: Point3D): Color = {
    val distance = b - a
    val fraction = point.x - floor(point.x)
    a + distance * fraction
  }

  override type PatternType = GradientPattern
  override def setTransform(t: Matrix): GradientPattern = this.copy(transform = t)
}

object GradientPattern {
  def apply(a: Color, b: Color, transform: Matrix = Matrix.identity): GradientPattern =
    new GradientPattern(a, b, transform)
}