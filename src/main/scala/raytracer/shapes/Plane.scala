package raytracer
package shapes

import scala.math.abs

/**
  * Play on the xz axis
  */
final class Plane(
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  //every single Point3D on plane has normal of (0, 1, 0)
  val constantNormal = Vector3D(0, 1, 0)

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = constantNormal

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (abs(ray.direction.y) < EPSILON) Nil
    else {
      val t = -ray.origin.y / ray.direction.y
      Seq(Intersection(t, this))
    }
  }

  override protected def calculateBounds: BoundingBox = Plane.Bounds

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Plane]

}

object Plane {
  def apply(transform: Matrix, material: Option[Material] = None): Plane =
    new Plane(transform, material)

  val Bounds = shapes.BoundingBox(Point3D(-INFINITY, 0, -INFINITY), Point3D(INFINITY, 0, INFINITY))
}
