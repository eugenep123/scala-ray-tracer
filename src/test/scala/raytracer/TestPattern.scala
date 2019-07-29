package raytracer

import raytracer.patterns.BasePattern

case class TestPattern(transform: Matrix = Matrix.identity) extends BasePattern {

  override def colorAt(point: Point3D): Color = {
    Color(point.x, point.y, point.z)
  }

  override type PatternType = TestPattern
  override def setTransform(t: Matrix): TestPattern =
    this.copy(transform = t)
}
