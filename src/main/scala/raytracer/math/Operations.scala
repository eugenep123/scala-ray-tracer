package raytracer.math

import java.lang.Math.{cos, sin}

object Operations {

  def identity: Matrix =
    Matrix(
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    )

  def translation(x: Double, y: Double, z: Double): Matrix =
    Matrix(
      1, 0, 0, x,
      0, 1, 0, y,
      0, 0, 1, z,
      0, 0, 0, 1
    )

  def scaling(x: Double, y: Double, z: Double): Matrix =
    Matrix(
      x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1
    )

  def rotationX(r: Radians): Matrix = Matrix(
    1,    0,      0,    0,
    0, cos(r), -sin(r), 0,
    0, sin(r),  cos(r), 0,
    0,    0,      0,    1
  )

  def rotationY(r: Radians): Matrix = Matrix(
    cos(r),   0,    sin(r), 0,
    0,        1,    0,      0,
    -sin(r),  0,    cos(r), 0,
    0,        0,    0,      1
  )

  def rotationZ(r: Radians): Matrix = Matrix(
    cos(r), -sin(r),  0, 0,
    sin(r),  cos(r),  0, 0,
    0,        0,      1, 0,
    0,        0,      0, 1
  )

  def shearing(xy: Int, xz: Int, yx: Int, yz: Int, zx: Int, zy: Int): Matrix = Matrix(
    1,  xy, xz, 0,
    yx, 1,  yz, 0,
    zx, zy, 1,  0,
    0,  0,  0,  1
  )

  def viewTransform(from: Point3D, to: Point3D, up: Vector3D): Matrix = {
    val forward = (to - from).normalize
    val upn = up.normalize
    val left = forward.cross(upn)
    val trueUp = left.cross(forward)
    val orientation = Matrix(
      left.x,     left.y,     left.z,     0,
      trueUp.x,   trueUp.y,   trueUp.z,   0,
      -forward.x, -forward.y, -forward.z, 0,
      0,          0,          0,          1
    )
    orientation * translation(-from.x, -from.y, -from.z)
  }
}
