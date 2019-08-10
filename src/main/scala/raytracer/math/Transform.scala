package raytracer
package math

sealed abstract class Transform extends Product with Serializable {
  def matrix: Matrix
}
object Transform {
  import Operations._

  def apply(): TransformBuilder = TransformBuilder(Nil)
  def from(m: Matrix): TransformBuilder = TransformBuilder(List(TransformMatrix(m)))
  case class TransformMatrix(matrix: Matrix) extends Transform

  case object Identity extends Transform {
    override def matrix: Matrix = identity
  }

  case class Translation(x: Double, y: Double, z: Double) extends Transform {
    override def matrix: Matrix = translation(x, y, z)
    override def toString: String = mkString("translation", x, y, z)
  }

  case class Scaling(x: Double, y: Double, z: Double) extends Transform {
    override def matrix: Matrix = scaling(x, y, z)
  }

  case class RotationX(r: Radians) extends Transform {
    override def matrix: Matrix = rotationX(r)
  }

  case class RotationY(r: Radians) extends Transform {
    override def matrix: Matrix = rotationY(r)
  }

  case class RotationZ(r: Radians) extends Transform {
    override def matrix: Matrix = rotationZ(r)
  }

  case class Shearing(xy: Int, xz: Int, yx: Int, yz: Int, zx: Int, zy: Int) extends Transform {
    override def matrix: Matrix = shearing(xy, xz, yx, yz, zx, zy)
  }

  case class ViewTransform(from: Point3D, to: Point3D, up: Vector3D) extends Transform {
    override def matrix: Matrix = viewTransform(from, to, up)
  }
}
