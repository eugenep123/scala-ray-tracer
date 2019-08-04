package raytracer
package shapes

import scala.math.{abs, max, min}

/**
  * Play on the xz
  */
final class Cube(
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  override def localNormalAt(point: Point3D, i: Intersection): Vector3D = {
    val maxC = max(max(abs(point.x), abs(point.y)), abs(point.z))
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

    val tmin = max(max(xtmin, ytmin), ztmin)
    val tmax = min(min(xtmax, ytmax), ztmax)

    if (tmin > tmax) Nil //miss
    else Seq(Intersection(tmin, this), Intersection(tmax, this))
  }

  override protected def calculateBounds: BoundingBox = Cube.Bounds

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Cube]
}

object Cube {
  val Bounds = shapes.BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

  def apply(
    transform: Option[Matrix] = None,
    material: Option[Material] = None): Cube = {
    new Cube(transform.getOrElse(Matrix.identity), material)
  }

  @inline final def checkAxis(origin: Double, direction: Double, min: Double, max: Double): (Double, Double) = {
    val positiveDirection =
      if (abs(direction) >= EPSILON) direction else INFINITY
    val tmin = (min - origin) / positiveDirection
    val tmax = (max - origin) / positiveDirection
    if (tmin > tmax) (tmax, tmin)
    else (tmin, tmax)
  }

}