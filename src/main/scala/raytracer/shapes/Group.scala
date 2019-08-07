package raytracer
package shapes

import javax.naming.OperationNotSupportedException
import raytracer.math._

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

//TODO: add name
final class Group(
  transform: Matrix,
  material: Option[Material]) extends Shape(transform, material) {

  private val _children: ListBuffer[Shape] = ListBuffer.empty

  def isEmpty: Boolean = _children.isEmpty
  def includes(s: Shape): Boolean = _children.contains(s)
  def size: Int = _children.size

  def children: Seq[Shape] = _children
  def collectChildren[A <: Shape : ClassTag]: Seq[A] =
    _children.collect { case a: A => a}

  def addChild(child: Shape): Group = {
    child.parent.foreach {
      case g: Group => g.removeChild(child)
    }
    child.setParent(Some(this))
    this._children.append(child)
    this
  }

  def addChildren(xs: Seq[Shape]): Group = {
    xs foreach addChild
    this
  }

  def removeChild(child: Shape): Unit = {
    if (child.parent.contains(this)) {
      child.setParent(None)
      this._children.remove(this._children.indexOf(child))
    }
  }

  def removeChildren(xs: Seq[Shape]): Unit = {
    xs foreach removeChild
  }

  // TODO: return this? or subgroup????
  def createSubGroup(xs: Seq[Shape]): Group = {
    val subGroup = Group().addChildren(xs)
    addChild(subGroup)
    subGroup
  }

  def partitionChildren: (Seq[Shape], Seq[Shape]) = {
    val (leftBounds, rightBounds) = bounds.split
    val left = children.filter(child => leftBounds.contains(child.boundsTransformed))
    removeChildren(left)
    val right = children.filter(child => rightBounds.contains(child.boundsTransformed))
    removeChildren(right)
    (left.toList, right.toList)
  }

  def divide(threshold: Int): Unit = {
    if (threshold <= size) {
      val (left, right) = partitionChildren
      if (!left.isEmpty) createSubGroup(left)
      if (!right.isEmpty) createSubGroup(right)
    }
    collectChildren[Group].foreach(_.divide(threshold))
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
    //TODO: ass optional visual bounding box (debug only)
//    add(box.toCube.setRenderAllRays(false))
//    box

    BoundingBox.of(_children)
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

}