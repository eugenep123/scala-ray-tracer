package raytracer
package shapes

import math._

class TestShape(
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  private var _savedRay = Option.empty[Ray]

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    this._savedRay = Some(ray)
    Nil
  }

  override def localNormalAt(p: Point3D, i: Intersection): Vector3D = {
    Vector3D(p.x, p.y, p.z)
  }

  override protected def calculateBounds: BoundingBox =
    BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

  override def canEqual(other: Any): Boolean = other.isInstanceOf[TestShape]

  def savedRay: Option[Ray] = _savedRay
}

object TestShape {
  def apply(
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): TestShape = {
    new TestShape(transform, material)
  }
}