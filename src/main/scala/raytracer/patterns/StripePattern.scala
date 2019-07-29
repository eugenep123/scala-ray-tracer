package raytracer.patterns

import raytracer.{Color, Matrix, Point3D}

case class StripePattern(a: Color, b: Color, transform: Matrix) extends BasePattern {
  override def colorAt(point: Point3D): Color = {
    if (math.floor(point.x).toInt % 2 == 0) a
    else b
  }

  override type PatternType = StripePattern
  override def setTransform(t: Matrix): StripePattern = this.copy(transform = t)
}

object StripePattern {
  def apply(a: Color, b: Color, transform: Matrix = Matrix.identity): StripePattern =
    new StripePattern(a, b, transform)
}