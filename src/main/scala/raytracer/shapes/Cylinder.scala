package raytracer
package shapes

import java.lang.Math.{abs, pow, sqrt}

import scala.collection.mutable.ListBuffer

/**
  * A cylinder along the y axis, with radius on 1
  */
final class Cylinder(
  minimum: Double,
  maximum: Double,
  closed: Boolean,
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    val xs = ListBuffer.empty[Intersection]
    val a = pow(ray.direction.x, 2) + pow(ray.direction.z, 2)
    // ray is not parallel to the y axis
    if (abs(a) > EPSILON) {
      val b = 2 * ray.origin.x * ray.direction.x +
        2 * ray.origin.z * ray.direction.z
      val c = pow(ray.origin.x, 2) + pow(ray.origin.z, 2) - 1
      val disc = pow(b, 2) - 4.0 * a * c

      // Ray does not intersect the cylinder
      if (disc >= 0) {
        var t0 = (-b - sqrt(disc)) / (2 * a)
        var t1 = (-b + sqrt(disc)) / (2 * a)
        if (t0 > t1) {
          // Swap
          val temp = t0
          t0 = t1
          t1 = temp
        }
        val y0 = ray.origin.y + t0 * ray.direction.y
        if (isInRange(y0)) {
          xs.append(Intersection(t0, this))
        }

        val y1 = ray.origin.y + t1 * ray.direction.y
        if (isInRange(y1)) {
          xs.append(Intersection(t1, this))
        }
      }
    }
    intersectCaps(ray, xs)
    xs
  }

  @inline private final def isInRange(t: Double): Boolean =
    minimum < t && t < maximum

  /**
    * Check if the intersection at 't' is within a radius of 1. (the radius of the cylinder) from the y axis
    */
  @inline private final def checkCap(ray: Ray, t: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    val radius = pow(x, 2) + pow(z, 2)
    radius <= 1.0
  }

  @inline private final def intersectCaps(ray: Ray, xs: ListBuffer[Intersection]): Unit = {
    // Caps only matter if the cylinder is close, and might possible be intersected by the ray
    if (!closed || abs(ray.direction.y) < EPSILON) {
      return
    }

    // check for n intersection with the lower end cap by intersecting
    // the ray with the plane at y = cyl.minimum
    val t1 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1)) {
      xs.append(Intersection(t1, this))
    }

    // Check the intersection with the upper end cape by intersecting
    // the ray with the plane at y = cyl.maximum
    val t2 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t2)) {
      xs.append(Intersection(t2, this))
    }
  }

  override def localNormalAt(point: Point3D, i: Intersection): Vector3D = {
    val dist = pow(point.x, 2) + pow(point.z, 2)
    if (dist < 1.0 && point.y >= (maximum - EPSILON)) Vector3D(0, 1, 0)
    else if (dist < 1.0 && point.y <= (minimum + EPSILON)) Vector3D(0, -1, 0)
    else Vector3D(point.x, 0, point.z)
  }

  override protected def calculateBounds: BoundingBox =
    BoundingBox(Point3D(-1, minimum, -1), Point3D(1, maximum, 1))

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Cylinder]

  override def hashCode:Int = {
    val hash1 = super.hashCode() * 41 + maximum.hashCode()
    val hash2 = hash1 * 41 + minimum.hashCode()
    hash2 * 41 + closed.hashCode()
  }
}

object Cylinder {
  def apply(
   minimum: Double = -INFINITY,
   maximum: Double = INFINITY,
   closed: Boolean = false,
   transform: Matrix = Matrix.identity,
   material: Option[Material] = None): Cylinder = {
    new Cylinder(minimum, maximum, closed, transform, material)
  }



}
