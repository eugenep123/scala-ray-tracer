package performance.obj

import raytracer.math.Matrix
import raytracer.shapes.{BoundingBox, Group, Shape, TriangleData}


trait MeshNode {
  def bounds: BoundingBox

}
case class MeshGroup(children: Seq[MeshNode]) extends MeshNode {
  lazy val bounds = children.foldLeft(BoundingBox.Empty)((bbox, c) => bbox.add(c.bounds))
}

case class MeshTriangle(t: TriangleData) extends MeshNode {
  override def bounds: BoundingBox = t.bounds
}

case class TriangleMesh(root: MeshNode, transform: Matrix) {
  def toShape: Group = ???
}



object TriangleMesh {
  def threshold: Int = 2
  def divide(node: MeshNode): MeshNode = {
    node match {
      case g: MeshGroup if g.children.size > threshold => split(g)
      case _ => node
    }
  }

  def split(group: MeshGroup): MeshGroup = {
    import group._
    val (leftBounds, rightBounds) = bounds.split
    val left = children.filter(child => leftBounds.contains(child.bounds))
    val right = children.filter(child => leftBounds.contains(child.bounds))
    val remaining = children.filterNot(c => left.exists(_ eq c) || right.exists(_ eq c))

    val dividedLeft = divide(MeshGroup(left))
    val dividedRight =divide(MeshGroup(right))

    val newChildren = Seq(dividedLeft, dividedRight) ++ remaining
    MeshGroup(newChildren)
  }


  def build(r: ObjResult): MeshNode = {
    val root = buildGroup(r)
    divide(root)
  }

  def buildGroup(r: ObjResult): MeshGroup = {

    var groups = Vector.empty[MeshGroup]
    var currentGroup: String = r.groups(0)
    var triangles = Vector.empty[TriangleData]

    def pushGroup(): Unit = {
      if (triangles.nonEmpty) {
        val group = MeshGroup(triangles.map(MeshTriangle))
        groups :+= group
        triangles = Vector.empty
      }
    }
    r.forEach { case (groupName, triangle) =>
      // Push New Group
      if (groupName != currentGroup ) {
        pushGroup()
        currentGroup = groupName
      }
      triangles :+= triangle
    }
    pushGroup()

    groups match {
      case Seq(a) => a
      case _ => MeshGroup(groups)
    }
  }

}

