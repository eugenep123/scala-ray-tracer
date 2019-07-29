package raytracer
package shapes

import scala.math.{abs, max, min}

/**
  * Play on the xz
  */
class Cube extends MutableShape {

  override def localNormalAt(point: Point3D): Vector3D = {
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
    val (xtmin, xtmax) = Cube.checkAxis(origin.x, direction.x)
    val (ytmin, ytmax) = Cube.checkAxis(origin.y, direction.y)
    val (ztmin, ztmax) = Cube.checkAxis(origin.z, direction.z)

    val tmin = max(max(xtmin, ytmin), ztmin)
    val tmax = min(min(xtmax, ytmax), ztmax)

    if (tmin > tmax) Nil //miss
    else Seq(Intersection(tmin, this), Intersection(tmax, this))
  }

  override def bounds: BoundingBox = Cube.Bounds
}

object Cube {
  def apply(
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape]): Cube = {
    new Cube().setTransform(transform).setMaterial(material).setParent(parent)
  }

  @inline final def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val tminNumerator = -1 - origin
    val tmaxNumerator = 1 - origin

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

  val Bounds = BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))

}