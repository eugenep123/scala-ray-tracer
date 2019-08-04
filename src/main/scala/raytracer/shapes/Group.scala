package raytracer
package shapes

import javax.naming.OperationNotSupportedException

import scala.collection.mutable.ListBuffer

class Group extends MutableShape {

  private val children: ListBuffer[Shape] = ListBuffer.empty

  def isEmpty: Boolean = children.isEmpty
  def includes(s: Shape): Boolean = children.contains(s)

  // Triangle children
  def triangles: Seq[Triangle] = children.collect { case t: Triangle => t }
  def smoothTriangles: Seq[SmoothTriangle] = children.collect { case t: SmoothTriangle => t }

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

  override def localNormalAt(localPoint: Point3D, hit: Intersection): Vector3D = {
    throw new OperationNotSupportedException("group does not support localNormalAt")
  }

  override def localIntersect(ray: Ray): Seq[Intersection] = {
    if (isEmpty || !bounds.intersects(ray)) Nil
    else {
      val xs = children.flatMap(_.intersect(ray))
      Intersection.sort(xs)
    }
  }

  private def calcBounds: BoundingBox = {
    val box = BoundingBox(children.filter(_.renderAllRays))
    // Add bounding box (no shadows or reflection/refraction)
//    add(box.toCube.setRenderAllRays(false))
    box
  }
  // TODO: cache this value
//  override def bounds: BoundingBox = BoundingBox(children)
  override lazy val bounds: BoundingBox = calcBounds
}

object Group {

  def apply(
    transform: Matrix = Matrix.identity,
    parent: Option[Shape] = None): Group = {
    new Group().setTransform(transform).setParent(parent)
  }

  def create(xs: Seq[Shape]): Group = {
    xs.size match {
      case 0 => Group()
      case 1 =>
        xs.head match {
          case g: Group => g
          case other =>
            val g = new Group()
            g.add(other)
            g
        }
      case _ =>
        val g = new Group()
        xs.foreach(g add _)
        g
    }
  }

}