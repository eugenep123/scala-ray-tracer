package raytracer
package shapes

import javax.naming.OperationNotSupportedException
import math._
import scala.collection.mutable.ListBuffer

final class Group(
                 //TODO: add name
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  private val _children: ListBuffer[Shape] = ListBuffer.empty

  def isEmpty: Boolean = _children.isEmpty
  def includes(s: Shape): Boolean = _children.contains(s)
  def size: Int = _children.size
  def children: Seq[Shape] = _children.toSeq

  // Triangle children
  def triangles: Seq[Triangle] = _children.collect { case t: Triangle => t }
  def smoothTriangles: Seq[SmoothTriangle] = _children.collect { case t: SmoothTriangle => t }

  def add(child: Shape): Group = {
    child.parent.foreach {
      case g: Group => g.remove(child)
    }
    child.setParent(Some(this))
    this._children.append(child)
    this
  }

  def remove(child: Shape): Unit = {
    if (child.parent.contains(this)) {
      child.setParent(None)
      this._children.remove(this._children.indexOf(child))
    }
  }

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
    throw new OperationNotSupportedException("group does not support localNormalAt")
  }

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (isEmpty || !bounds.intersects(ray)) Nil
    else {
      val xs = _children.flatMap(_.intersect(ray))
      Intersection.sort(xs)
    }
  }


  override protected def calculateBounds: BoundingBox = {
//    val box = BoundingBox(children.filter(_.renderAllRays))
    // Add bounding box (no shadows or reflection/refraction)
//    add(box.toCube.setRenderAllRays(false))
//    box
    BoundingBox(_children)
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Group]

  override def hashCode:Int = {
    val ourHash = _children.map(_.hashCode).sum
    super.hashCode + ourHash + ourHash
  }
}

object Group {

  def apply(
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Group = {
    new Group(transform, material)
  }

  def create(xs: Seq[Shape]): Group = {
    xs.size match {
      case 0 => Group()
      case 1 =>
        xs.head match {
          case g: Group => g
          case other =>
            val g = Group()
            g.add(other)
            g
        }
      case _ =>
        val g = Group()
        xs.foreach(g add _)
        g
    }
  }

}