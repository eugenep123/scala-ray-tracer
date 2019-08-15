package raytracer.math
import Transforms._

case class TransformBuilder(operations: List[Transforms]) {

  def add(op: Transforms): TransformBuilder =
    TransformBuilder(op :: operations)

  def identity: TransformBuilder = add(Identity)

  def translate(v: Vector3D): TransformBuilder = translate(v.x, v.y, v.z)
  def translate(x: Double, y: Double, z: Double): TransformBuilder = add(Translation(x, y, z))

  def scale(v: Vector3D): TransformBuilder = scale(v.x, v.y, v.z)
  def scale(n: Double): TransformBuilder = scale(n, n, n)
  def scale(x: Double, y: Double, z: Double): TransformBuilder = add(Scaling(x, y, z))

  def rotateX(r: Radians): TransformBuilder = add(RotationX(r))

  def rotateY(r: Radians): TransformBuilder = add(RotationY(r))

  def rotateZ(r: Radians): TransformBuilder = add(RotationZ(r))

  def shear(xy: Int, xz: Int, yx: Int, yz: Int, zx: Int, zy: Int): TransformBuilder =
    add(Shearing(xy, xz, yx, yz, zx, zy))

  def build(): Matrix = {
    if (operations.isEmpty) Operations.identity
    else operations
      .map(_.matrix)
      .reduce(_ * _)
  }

  override def toString: String =
    operations.reverse.mkString("Builder(", ", ", ")")
}

