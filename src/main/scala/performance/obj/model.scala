package performance.obj

import raytracer.math._


case class VertexIndex(
  vertex: Int,
  normal: Int = -1,
  texture: Int = -1)

case class Face(
  material: Int,
  group: Int,
  xs: Seq[VertexIndex])

case class ObjResult(
  materials: Vector[String],
  groups: Vector[String],
  vertices: Vector[Point3D],
  normals: Vector[Vector3D],
  textureCoords: Vector[(Double, Double)],
  faces: Vector[Face])


