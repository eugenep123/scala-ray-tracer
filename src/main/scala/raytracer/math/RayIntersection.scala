package raytracer.math

import raytracer.shapes.Shape

import scala.math.{pow, sqrt}

case class RayIntersection(
  t: Double,
  shape: Shape,
  point: Point3D,
  overPoint: Point3D,
  underPoint: Point3D,
  eye: Vector3D,
  normal: Vector3D,
  reflectV: Vector3D,
  inside: Boolean,
  n1: Double,
  n2: Double) {

  val material = shape.material
  lazy val reflectance = schlick()

//  lazy val reflectance = schlick()
//  val reflectance: Double = schlick()


  @inline def schlick(): Double = {
    // find the cosine of the angle between the eye and normal vectors
    var cos = eye.dot(normal)
    // total internal reflection con only occur if n1 > n2
    if (n1 > n2) {
      val n = n1 / n2
      val sin2T = (n * n) * (1.0 - (cos * cos))
      if (sin2T > 1.0) {
        return 1.0
      }
      cos = sqrt(1.0 - sin2T)
    }
    val r = (n1 - n2) / (n1 + n2)
    val r0 = r * r
    r0 + (1 - r0) * pow((1 - cos), 5)
  }

}