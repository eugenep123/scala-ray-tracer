package raytracer.patterns

import raytracer.math.{Point3D, UVMaps}
import raytracer.shapes.Cube.Face

sealed trait UVMapping {
  def map(point: Point3D): (Double, Double)
}
object UVMapping {
  import UVMaps._

  case object Spherical extends UVMapping {
    override def map(point: Point3D): (Double, Double) = spherical(point)
  }
  case object Planar extends UVMapping {
    override def map(point: Point3D): (Double, Double) = planar(point)
  }
  case object Cylindrical extends UVMapping {
    override def map(point: Point3D): (Double, Double) = cylindrical(point)
  }
  case object Cube extends UVMapping {
    override def map(point: Point3D): (Double, Double) = {
      val face = raytracer.shapes.Cube.face(point)
      cube(face, point)
    }
    def map(face: Face, point: Point3D): (Double, Double) = cube(face, point)
  }
}