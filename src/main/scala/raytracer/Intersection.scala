package raytracer

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
  def hit(xs: Seq[Intersection], allShapes: Boolean = true): Option[Intersection] = {
    val ys = if (allShapes) xs else xs.filter(_.obj.renderAllRays)
    ys.find(_.t > 0)
  }
}