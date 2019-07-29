package raytracer
package shapes

import scala.math.abs

/**
  * Play on the xz axis
  */
class Plane extends MutableShape {

  //every single Point3D on plane has normal of (0, 1, 0)
  val constantNormal = Vector3D(0, 1, 0)

  override def localNormalAt(localPoint: Point3D): Vector3D = constantNormal

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (abs(ray.direction.y) < EPSILON) Nil
    else {
      val t = -ray.origin.y / ray.direction.y
      Seq(Intersection(t, this))
    }
  }

  override def bounds: BoundingBox = Plane.Bounds
}

object Plane {
  def apply(
    transform: Matrix = Matrix.identity,
    material: Material = Material(),
    parent: Option[Shape] = None): Plane = {
    new Plane()
      .setTransform(transform)
      .setMaterial(material)
      .setParent(parent)
  }

  val Bounds = BoundingBox(Point3D(-INFINITY, 0, -INFINITY), Point3D(INFINITY, 0, INFINITY))
}
