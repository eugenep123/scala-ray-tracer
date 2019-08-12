package raytracer.math

import raytracer.shapes.Cube
import raytracer.shapes.Cube.Face

import scala.math.{acos, atan2}

object UVMaps {


  @inline private final def clamp(u: Double, v: Double): (Double, Double)  =
    (if (u < 0.0) 1.0 + u else u, if (v < 0.0) 1.0 + v else v)

  def spherical(p: Point3D): (Radians, Radians) = {
    // compute the azimuthal angle
    // -π < theta <= π
    // angle increases clockwise as viewed from above,
    // which is opposite of what we want, but we'll fix it later.
    val theta = atan2(p.x, p.z)

    // vec is the vector pointing from the sphere's origin (the world origin)
    // to the point, which will also happen to be exactly equal to the sphere's
    // radius.
    val vec = Vector3D(p.x, p.y, p.z)
    val radius = vec.magnitude

    // compute the polar angle
    // 0 <= phi <= π
    val phi = acos(p.y / radius)

    // -0.5 < raw_u <= 0.5
    val rawU = theta / (2 * π)

    // 0 <= u < 1
    // here's also where we fix the direction of u. Subtract it from 1,
    // so that it increases counterclockwise as viewed from above.
    val u = 1 - (rawU + 0.5)

    // we want v to be 0 at the south pole of the sphere,
    // and 1 at the north pole, so we have to "flip it over"
    // by subtracting it from 1.
    val v = 1 - phi / π

    (u, v)
  }


  def planar(p: Point3D): (Double, Double) = {
    val u = p.x % 1.0
    val v = p.z % 1.0
    clamp(u, v)
  }

  def cylindrical(p: Point3D): (Radians, Radians) = {
    // compute the azimuthal angle, same as with spherical_map()
    val theta = atan2(p.x, p.z)
    val raw_u = theta / (2 * π)
    val u = 1 - (raw_u + 0.5)

    // let v go from 0 to 1 between whole units of y
    val v = p.y % 1

    clamp(u, v)
  }

  def cubeFront(p: Point3D): (Double, Double) = {
    val u = ((p.x + 1) % 2.0) / 2.0
    val v = ((p.y + 1) % 2.0) / 2.0
    clamp(u, v)
  }

  def cubeBack(p: Point3D): (Double, Double) = {
    val u = ((1 - p.x) % 2.0) / 2.0
    val v = ((p.y + 1) % 2.0) / 2.0
    clamp(u, v)
  }

  def cubeLeft(p: Point3D): (Double, Double) = {
    val u = ((p.z - 1) % 2.0) / 2.0
    val v = ((p.y + 1) % 2.0) / 2.0
    clamp(u, v)
  }

  def cubeRight(p: Point3D): (Double, Double) = {
    val u = ((1 - p.z) % 2.0) / 2.0
    val v = ((p.y + 1) % 2.0) / 2.0
    clamp(u, v)
  }

  def cubeUp(p: Point3D): (Double, Double) = {
    val u = ((p.x + 1) % 2.0) / 2.0
    val v = ((1 - p.z) % 2.0) / 2.0
    clamp(u, v)
  }

  def cubeDown(p: Point3D): (Double, Double) = {
    val u = ((p.x - 1) % 2.0) / 2.0
    val v = ((p.z + 1) % 2.0) / 2.0
    clamp(u, v)
  }

  def cube(face: Cube.Face, point: Point3D): (Double, Double) = {
    face match {
      case Face.Left => cubeLeft(point)
      case Face.Front => cubeFront(point)
      case Face.Right => cubeRight(point)
      case Face.Back => cubeBack(point)
      case Face.Up => cubeUp(point)
      case Face.Down => cubeDown(point)
    }
  }

}
