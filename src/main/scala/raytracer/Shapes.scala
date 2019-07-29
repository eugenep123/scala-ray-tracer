package raytracer

import raytracer.shapes._
import Defaults.Materials

// Shape factory
trait Shapes {
  private val Transform = Matrix.identity
  private val Material = Defaults.material

  final def sphere(
    transform: Matrix = Transform,
    material: Material = Material,
    parent: Option[Shape] = None): Sphere = Sphere(transform, material, parent)

  final def glassSphere(): Sphere = sphere(material = Materials.glass)


  def plane(
    transform: Matrix = Transform,
    material: Material = Material,
    parent: Option[Shape] = None): Plane = Plane(transform, material, parent)

  def cube(
    transform: Matrix = Transform,
    material: Material = Material,
    parent: Option[Shape] = None): Cube = Cube(transform, material, parent)

  def cylinder(
    transform: Matrix = Transform,
    material: Material = Material,
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false,
    parent: Option[Shape] = None): Cylinder =
    Cylinder(transform, material, minimum, maximum, closed, parent)

  def cone(
    transform: Matrix = Transform,
    material: Material = Material,
    minimum: Double = -INFINITY,
    maximum: Double = INFINITY,
    closed: Boolean = false,
    parent: Option[Shape] = None): Cone =
    Cone(transform, material, minimum, maximum, closed, parent)

  def group(
    transform: Matrix = Transform,
    parent: Option[Shape] = None): Group = Group(transform, parent)


}

object Shapes extends Shapes