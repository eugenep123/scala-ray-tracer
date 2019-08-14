package performance

import raytracer.math.{Point3D, Vector3D}
import raytracer.shapes.BoundingBox

object TriangleMesh {

    //TODO:
    // normalize faces
    // smooth normals?
    //Linear bounding volume hierarchies (LBVHs)
}

// Sharable triangle face
case class Face(
  p1: Point3D,
  p2: Point3D,
  p3: Point3D,
  n1: Vector3D,
  n2: Vector3D,
  n3: Vector3D)

case class TriangleMesh(
  faces: IndexedSeq[Face])


case class RawFace(

                  )