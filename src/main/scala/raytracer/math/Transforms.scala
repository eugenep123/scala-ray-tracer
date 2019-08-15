package raytracer
package math

sealed abstract class Transforms extends Product with Serializable {
  def matrix: Matrix
}
object Transforms {
  import Operations._

  def apply(): TransformBuilder = TransformBuilder(Nil)
  def from(m: Matrix): TransformBuilder = TransformBuilder(List(TransformMatrix(m)))
  case class TransformMatrix(matrix: Matrix) extends Transforms

  case object Identity extends Transforms {
    override def matrix: Matrix = identity
  }

  case class Translation(x: Double, y: Double, z: Double) extends Transforms {
    override def matrix: Matrix = translation(x, y, z)
    override def toString: String = mkString("translation", x, y, z)
  }

  case class Scaling(x: Double, y: Double, z: Double) extends Transforms {
    override def matrix: Matrix = scaling(x, y, z)
  }

  case class RotationX(r: Radians) extends Transforms {
    override def matrix: Matrix = rotationX(r)
  }

  case class RotationY(r: Radians) extends Transforms {
    override def matrix: Matrix = rotationY(r)
  }

  case class RotationZ(r: Radians) extends Transforms {
    override def matrix: Matrix = rotationZ(r)
  }

  case class Shearing(xy: Int, xz: Int, yx: Int, yz: Int, zx: Int, zy: Int) extends Transforms {
    override def matrix: Matrix = shearing(xy, xz, yx, yz, zx, zy)
  }

  case class ViewTransform(from: Point3D, to: Point3D, up: Vector3D) extends Transforms {
    override def matrix: Matrix = viewTransform(from, to, up)
  }
}
