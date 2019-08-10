package raytracer.math

import raytracer.shapes.Shape

// u/v is only used for smooth triangles
case class Intersection(
  t: Double,
  obj: Shape,
  u: Double = 0.0,
  v: Double = 0.0) {

  @inline private final def calculateRefractive(xs: Seq[Intersection]): (Double, Double) = {
    var n1 = 1.0
    var n2 = 1.0
    if (xs.size >= 1) {
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


  def prepare(ray: Ray, xs: Seq[Intersection]): RayIntersection = {
    val (n1, n2) = calculateRefractive(xs)
    val point = ray.position(t)
    val eye = -ray.direction
    var normal = obj.normalAt(point, this)
    var inside = false
    if (normal.dot(eye) < 0) {
      inside = true
      normal = -normal
    }

    val overPoint = point + normal * EPSILON
    val underPoint = point - normal * EPSILON
    val reflect = ray.direction.reflect(normal)

    RayIntersection(t, obj, point, overPoint, underPoint, eye, normal, reflect, inside, n1, n2)
  }
}

object Intersection {
  // Always make sure the hit is optimized
  def sort(xs: Seq[Intersection]): Seq[Intersection] = xs.sortBy(_.t)
  def hit(xs: Seq[Intersection]): Option[Intersection] = {
    xs.find(_.t > 0)
  }
}
