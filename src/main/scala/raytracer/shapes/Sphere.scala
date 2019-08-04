package raytracer
package shapes

import java.lang.Math.{pow, sqrt}

class Sphere extends MutableShape {

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    // vector from the sphere's center, to the ray origin
    // remember: the sphere is centered at the world origin
    val sphereToRay = ray.origin - Point3D.origin
    val a = ray.direction.dot(ray.direction)
    val b = 2 * ray.direction.dot(sphereToRay)
    val c = sphereToRay.dot(sphereToRay) - 1
    val discriminant = pow(b, 2) - 4 * a * c

    if (discriminant < 0) Nil
    else {
      val t1 = (-b - sqrt(discriminant)) / (2 * a)
      val t2 = (-b + sqrt(discriminant)) / (2 * a)
      Seq(
        raytracer.Intersection(t1, this),
        raytracer.Intersection(t2, this)
      )
    }
  }

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
    localPoint - Point3D.origin
  }

  override def bounds: BoundingBox = Sphere.Bounds
}

object Sphere {
  def apply(
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape] = None): Sphere = {
    new Sphere()
      .setTransform(transform)
      .setMaterial(material)
      .setParent(parent)
  }

  val Bounds = BoundingBox(Point3D(-1, -1, -1), Point3D(1, 1, 1))
}