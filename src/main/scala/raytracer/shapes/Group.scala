package raytracer
package shapes

import javax.naming.OperationNotSupportedException

import scala.collection.mutable.ListBuffer

class Group extends MutableShape {

  private val children: ListBuffer[Shape] = ListBuffer.empty

  def isEmpty: Boolean = children.isEmpty
  def includes(s: Shape): Boolean = children.contains(s)

  def add(child: Shape): Group = {
    child.parent
      .collect { case g: Group => g }
      .foreach(_.remove(child))

    child.setParent(Some(this))
    this.children.append(child)
    this
  }

  def remove(child: Shape): Unit = {
    if (child.parent.contains(this)) {
      child.setParent(None)
      this.children.remove(this.children.indexOf(child))
    }
  }

  override def localNormalAt(localPoint: Point3D): Vector3D = {
    throw new OperationNotSupportedException("group does not support localNormalAt")
  }

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (isEmpty) Nil
    else {
      val xs = children.flatMap(_.intersect(ray))
      Intersection.sort(xs)
    }
  }

  override def bounds: BoundingBox = {
    ???
  }
}

object Group {

  def apply(
    transform: Matrix = Matrix.identity,
    parent: Option[Shape] = None): Group = {
    new Group().setTransform(transform).setParent(parent)
  }


}