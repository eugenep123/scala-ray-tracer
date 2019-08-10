package raytracer
package shapes

import math._
import raytracer.shapes.Csg._


// Shape factory
trait Shapes {
  private val Transform = Matrix.identity

  def sphere(
    transform: Matrix = Transform,
    material: Option[Material] = None): Sphere = Sphere(transform, material)

  def glassSphere(transform: Matrix = Transform): Sphere = sphere(transform, Some(Material.Glass))

  def plane(
    transform: Matrix = Transform,
    material: Option[Material] = None): Plane = Plane(transform, material)

  def cube(
    transform: Matrix = Transform,
    material: Option[Material] = None): Cube = Cube(transform, material)

  def cylinder(
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false,
    transform: Matrix = Transform,
    material: Option[Material] = None): Cylinder =
    Cylinder(minimum, maximum, closed, transform, material)

  def cone(
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false,
    transform: Matrix = Transform,
    material: Option[Material] = None): Cone =
    Cone(minimum, maximum, closed, transform, material)

  def group(
    transform: Matrix = Transform): Group = Group(transform)

  def triangle(
    p1: Point3D,
    p2: Point3D,
    p3: Point3D,
    transform: Matrix = Transform,
    material: Option[Material] = None): Triangle = Triangle(p1, p2, p3, transform, material)

  def smoothTriangle(
    p1: Point3D, p2: Point3D, p3: Point3D,
    n1: Vector3D, n2: Vector3D, n3: Vector3D,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): SmoothTriangle = {
    SmoothTriangle(p1, p2, p3, n1, n2, n3, transform, material)
  }

  def csg(
    operation: CsgOperation, left: Shape, right: Shape,
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): Csg = {
    Csg(operation, left, right, transform, material)
  }

  def testShape(
    transform: Matrix = Matrix.identity,
    material: Option[Material] = None): TestShape = TestShape(transform, material)

}

object Shapes extends Shapes