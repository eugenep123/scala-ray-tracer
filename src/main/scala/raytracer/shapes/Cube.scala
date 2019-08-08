package raytracer
package shapes

import raytracer.math._
import scala.math.abs

/**
  * Play on the xz
  */
final class Cube(
  transform: Matrix,
  material: Option[Material],
  castsShadow: Boolean) extends Shape(transform, material, castsShadow) {

  override def localNormalAt(point: Point3D, i: Intersection): Vector3D = {
    val maxC = max(abs(point.x), abs(point.y), abs(point.z))
    if (maxC == abs(point.x))
      Vector3D(point.x, 0, 0)
    else if (maxC == abs(point.y))
      Vector3D(0, point.y, 0)
    else
      Vector3D(0, 0, point.z)
  }

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    import ray.{direction, origin}
    val (xtmin, xtmax) = Cube.checkAxis(origin.x, direction.x, -1, 1)
    val (ytmin, ytmax) = Cube.checkAxis(origin.y, direction.y, -1, 1)
    val (ztmin, ztmax) = Cube.checkAxis(origin.z, direction.z, -1, 1)

    val tmin = max(xtmin, ytmin, ztmin)
    val tmax = min(xtmax, ytmax, ztmax)

    if (tmin > tmax) Nil //miss
    else Seq(Intersection(tmin, this), Intersection(tmax, this))
  }

  override protected def calculateBounds: BoundingBox = Cube.Bounds

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Cube]
}

object Cube {
  val Bounds = shapes.BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

  def apply(
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None,
    castsShadow: Boolean = true): Cube = {
    new Cube(transform, material, castsShadow)
  }

  @inline final def checkAxis(origin: Double, direction: Double, min: Double, max: Double): (Double, Double) = {
    val tminNumerator = min - origin
    val tmaxNumerator = max - origin

    if (abs(direction) >= EPSILON) {
      val tmin = tminNumerator / direction
      val tmax = tmaxNumerator / direction
      inOrder(tmin, tmax)
    } else {
      val tmin = tminNumerator * INFINITY
      val tmax = tmaxNumerator * INFINITY
      inOrder(tmin, tmax)
    }
  }

  @inline private final def inOrder(tmin: Double, tmax: Double): (Double, Double) = {
    if (tmin > tmax) (tmax, tmin)
    else (tmin, tmax)
  }

}