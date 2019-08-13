package raytracer.shapes

import raytracer.Material
import raytracer.math.{Intersection, Matrix, Point3D, Ray, Vector3D}
import raytracer.shapes.Csg._

final class Csg(
  val operation: CsgOperation,
  val left: Shape,
  val right: Shape,
  transform: Matrix = Matrix.identity,
  material: Option[Material] = None) extends Shape(transform, material) {

  left.setParent(this)
  right.setParent(this)

  final override def includes(s: Shape): Boolean =
    left.includes(s) || right.includes(s)

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = ???

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (!bounds.intersects(ray)) Nil
    else {
      val lefts = left.intersect(ray)
      val rights = right.intersect(ray)
      val xs = (lefts ++ rights).sortBy(_.t) //Must be sorted
      filterIntersections(xs)
    }
  }

  override protected def calculateBounds: BoundingBox = {
    BoundingBox.Empty.add(left.parentSpaceBounds).add(right.parentSpaceBounds)
  }

  def filterIntersections(xs: Seq[Intersection]): Seq[Intersection] = {
    // prepare a list to receive the filtered intersections
    var result: Seq[Intersection] = Nil
    var inl = false
    var inr = false
    xs foreach { i =>
      //# if i.object is part of the "left" child, then lhit is true
      val lhit = left.includes(i.obj)

      if (operation.allowed(lhit, inl, inr)) {
        result = result :+ i
      }

      //depending on which object was hit, toggle either inl or inr
      if (lhit)
        inl = !inl
      else
        inr = ! inr
    }
    result
  }

  override def divide(threshold: Int): Unit = {
    left.divide(threshold)
    right.divide(threshold)
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Csg]

  override def hashCode: Int = {
    (super.hashCode, operation, left, right).hashCode()
  }
}

object Csg {

  def apply(
    op: CsgOperation,
    left: Shape,
    right: Shape,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Csg = {
    new Csg(op, left, right, transform, material)
  }

  sealed trait CsgOperation {
    /*
     * lhit is true if the left shape was hit, and false if the right shape was hit.
     * inl is true if the hit occurs inside the left shape.
     * inr is true if the hit occurs inside the right shape.
     */
    def allowed(lhit: Boolean, inl: Boolean, inr: Boolean): Boolean
  }

  case object Union extends CsgOperation {
    override def allowed(lhit: Boolean, inl: Boolean, inr: Boolean): Boolean =
      (lhit && !inr) || (!lhit && !inl)
  }

  case object Intersection extends CsgOperation {
    override def allowed(lhit: Boolean, inl: Boolean, inr: Boolean): Boolean =
      (lhit && inr) || (!lhit && inl)
  }

  case object Difference extends CsgOperation {
    override def allowed(lhit: Boolean, inl: Boolean, inr: Boolean): Boolean =
      (lhit && !inr) || (!lhit && inl)
  }


  def unionAll(xs: Seq[Shape], mat: Material): Option[Shape] = {
    def reduce(ys: List[Shape]): Option[Shape] = {
      ys match {
        case Nil => None
        case h :: Nil => ys.headOption
        case a :: b :: Nil =>
          Some(Csg(Union, a, b, material = mat))
        case _ =>
          val merged = ys.grouped(2).map {
            case Seq(a, b) => Csg(Union, a, b)
            case Seq(a) => a
          }
          reduce(merged.toList)
      }
    }
    reduce(xs.toList)
  }
}