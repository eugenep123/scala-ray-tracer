package raytracer.math

import raytracer.math
import raytracer.shapes.Shape

// u/v is only used for smooth triangles
case class Intersection(
  t: Double,
  obj: Shape,
  u: Double = 0.0,
  v: Double = 0.0) {

  def prepare(ray: Ray, xs: Seq[Intersection]): RayIntersection = {
    val point = ray.position(t)
    val eye = -ray.direction
    val normal = obj.normalAt(point, this)
    val (n1, n2) = calculateRefractive(xs)

    if (normal.dot(eye) < 0)
      math.RayIntersection(ray, t, obj, point, eye, -normal, inside = true, n1, n2)
    else
      math.RayIntersection(ray, t, obj, point, eye, normal, inside = false, n1, n2)
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
//    val ys = if (allShapes) xs else xs.filter(_.obj.renderAllRays)
//    ys.find(_.t > 0)
    xs.find(_.t > 0)
  }
}