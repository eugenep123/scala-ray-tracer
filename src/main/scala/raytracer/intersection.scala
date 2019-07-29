package raytracer

import math.pow

case class Intersection(t: Double, obj: Shape) {
  def prepare(ray: Ray, xs: Seq[Intersection]): RayIntersection = {
    val Point3D = ray.position(t)
    val eye = -ray.direction
    val normal = obj.normalAt(Point3D)
    val (n1, n2) = calculateRefractive(xs)

    if (normal.dot(eye) < 0)
      RayIntersection(ray, t, obj, Point3D, eye, -normal, inside = true, n1, n2)
    else
      RayIntersection(ray, t, obj, Point3D, eye, normal, inside = false, n1, n2)
  }

  @inline private final def calculateRefractive(xs: Seq[Intersection]): (Double, Double) = {
    var n1 = 1.0
    var n2 = 1.0
    if (xs.size > 1) {
      val containers = scala.collection.mutable.ArrayBuffer.empty[Shape]
      xs.takeWhile { intersection =>
        val shape = intersection.obj
        val isHit = (intersection eq this)
        if (isHit) {
          n1 =
            if (containers.isEmpty) 1.0
            else containers.last.material.refractiveIndex
        }

        if (containers.contains(shape)) containers.remove(containers.indexOf(shape))
        else containers.append(shape)
        if (isHit) {
          n2 =
            if (containers.isEmpty) 1.0
            else containers.last.material.refractiveIndex
        }
        !isHit
      }
    }
    (n1, n2)
  }
}

object Intersection {
  // Always make sure the hit is optimized
  def sort(xs: Seq[Intersection]): Seq[Intersection] = xs.sortBy(_.t)
  def hit(xs: Seq[Intersection]): Option[Intersection] = xs.find(_.t > 0)
}

// Cached intersection computation
case class RayIntersection(
  ray: Ray,
  t: Double,
  obj: Shape,
  point: Point3D,
  eye: Vector3D,
  normal: Vector3D,
  inside: Boolean,
  n1: Double, n2: Double) {

  val overPoint: Point3D = point + normal * EPSILON
  val underPoint: Point3D = point - normal * EPSILON

  // After negating the normal, if necessary
  def reflectV = Vector3D.reflect(ray.direction, normal)
  lazy val reflectiveRay: Ray = Ray(overPoint, reflectV)

  lazy val reflectance: Double = schlick()
  def schlick(): Double = {
    val Fail = 0.0
    // find the cosine of the angle between the eye and normal vectors
    val cos = eye.dot(normal)
    // total internal reflection con only occur if n1 > n2
    if (n1 > n2) {
      val n = n1 / n2
      val sin2t = pow(n, 2) * (1.0 - pow(cos, 2))
      if (sin2t > 1.0) 1.0 else Fail
    } else {
      // return anything but 1.0 here, so that the test will fail
      // appropriately if something goes wrong
      Fail
    }
  }
}
