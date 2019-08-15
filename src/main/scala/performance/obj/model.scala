package performance.obj

import raytracer.math._
import raytracer.shapes.{BoundingBox, Group, Triangle, TriangleData}

case class VertexIndex(
  vertex: Int,
  texture: Int = -1,
  normal: Int = -1)

case class Face(
  material: Int,
  group: Int,
  xs: Seq[VertexIndex])


case class BVHInfo(index: Int, bounds: BoundingBox) {
  val centroid: Point3D = bounds.center
}

case class ObjResult(
  materials: Vector[String],
  groups: Vector[String],
  vertices: Vector[Point3D],
  normals: Vector[Vector3D],
  textureCoords: Vector[(Double, Double)],
  faces: Vector[Face]) {

  def forEach(f: (String, TriangleData) => Unit): Unit = {
    faces.zipWithIndex.foreach { case (face, index) =>
      val group = groups(face.group)
      val hasNormals = face.xs.headOption.exists(_.normal != -1)
      val triangles = if (hasNormals) smooth(face.xs) else normal(face.xs)
      triangles.foreach(t => f(group, t))
    }
  }

  def smooth(xs: Seq[VertexIndex]): Seq[TriangleData] = {
    val points = xs.map(vi => (vertices(vi.vertex - 1), normals(vi.normal - 1)))
    TriangleData.fanTriangulationS(points)
  }

  def normal(xs: Seq[VertexIndex]): Seq[TriangleData] = {
    val points = xs.map(vi => vertices(vi.vertex - 1))
    TriangleData.fanTriangulation(points)
  }
  def normalize: ObjResult = {
    val normalized = Point3D.normalize(vertices)
    copy(vertices = normalized)
  }

  def toGroup() = {
    val root = Group()
    var current = root
    var currentName = "default-group"
    forEach { case (name, triangle) =>
      if (name != currentName ) {
        if (!current.isEmpty) {
          root.addChild(current)
          current = Group()
        }
        currentName = name
      }
      current.addChild(Triangle.data(triangle))
    }
    if (!current.isEmpty && (root ne current)) {
      root.addChild(current)
    }
    root
  }
}



