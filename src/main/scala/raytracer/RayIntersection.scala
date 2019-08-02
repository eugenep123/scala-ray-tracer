package raytracer

import scala.math.pow


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

  val renderAllRays: Boolean = obj.renderAllRays

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
