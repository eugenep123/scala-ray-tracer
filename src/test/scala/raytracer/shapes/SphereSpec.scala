package raytracer
package shapes

import java.lang.Math.sqrt
import raytracer.math._

class SphereSpec extends BaseSpec {

  feature("Spheres") {
    scenario("A ray intersects a sphere at two points") {
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      When("xs ← intersect(s, r)")
      val s = sphere()
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs[0] = 4.0")
      assert(xs.head.t == 4.0)
      And(" xs(1) = 6.0")
      assert(xs(1).t == 6.0)
    }

    scenario("A ray intersects a sphere at a tangent") {
      val r = ray(point(0, 1, -5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      val s = sphere()
      When("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs[0] = 5.0")
      assert(xs.head.t == 5.0)
      And("xs(1) = 5.0")
      assert(xs(1).t == 5.0)
    }

    scenario("A ray misses a sphere") {
      val r = ray(point(0, 2, -5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      val s = sphere()
      When("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 0")
      assert(xs.isEmpty)
    }

    scenario("A ray originates inside a sphere") {
      val r = ray(point(0, 0, 0), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      val s = sphere()
      When("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs[0] = -1.0")
      assert(xs.head.t == -1.0)
      And("xs(1) = 1.0 ")
      assert(xs(1).t == 1.0)
    }

    scenario("A sphere is behind a ray") {
      val r = ray(point(0, 0, 5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      val s = sphere()
      When("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs[0] = -6.0")
      assert(xs.head.t == -6.0)
      And("xs(1) = -4.0")
      assert(xs(1).t == -4.0)
    }

    scenario("Intersect sets the object on the intersection") {
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      val s = sphere()
      When("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs(0).object = s")
      assert(xs(0).obj == s)
      And("xs(1).object = s")
      assert(xs(1).obj == s)
    }

    scenario("A sphere's default transformation") {
      Given("s ← sphere()")
      val s = sphere()
      Then("s.transform = identity_matrix")
      assert(s.transform == identityMatrix)
    }

    scenario("Changing a sphere's transformation") {
      Given("s ← sphere()")
      And(s"t ← translation(2, 3, 4)")
      When("set_transform(s, t)")
      Then("s.transform = t")
      val t = translation(2, 3, 4)
      val s = sphere(t)
      assert(s.transform == t.matrix)
    }

    scenario("Intersecting a scaled sphere with a ray") {
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("s ← sphere()")
      When("set_transform(s, scaling(2, 2, 2))")
      val s = sphere(scaling(2, 2, 2))
      And("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 2")
      assert(xs.size == 2)
      And("xs(0).t = 3")
      assert(xs(0).t == 3)
      And("xs(1).t = 7")
      assert(xs(1).t == 7)
    }

    scenario("Intersecting a translated sphere with a ray") {
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given(s"r ← $r")
      And("s ← sphere()")
      When("set_transform(s, translation(5, 0, 0))")
      val s = sphere(translation(5, 0, 0))
      And(" xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("xs.size = 0")
      assert(xs.isEmpty)
    }

    scenario("The normal on a sphere at a point on the x axis") {
      And("s ← sphere()")
      val s = sphere()
      When("n ← normal_at(s, point(1, 0, 0))")
      val n = normalAt(s, point(1, 0, 0))
      Then("n = vector(1, 0, 0)")
      assert(n == vector(1, 0, 0))
    }

    scenario("The normal on a sphere at a point on the y axis") {
      And("s ← sphere()")
      val s = sphere()
      When("n ← normalAt(s, point(0, 1, 0))")
      val n = normalAt(s, point(0, 1, 0))
      Then("n = vector(0, 1, 0)")
      assert(n == vector(0, 1, 0))
    }

    scenario("The normal on a sphere at a point on the z axis") {
      And("s ← sphere()")
      val s = sphere()
      When("n  ← normalAt(s, point(0, 0, 1))")
      val n = normalAt(s, point(0, 0, 1))
      Then("n = vector(0, 0, 1)")
      assert(n == vector(0, 0, 1))
    }

    scenario("The normal on a sphere at a non-axial point") {
      And("s ← sphere()")
      val s = sphere()
      When("n ← normalAt(s, point(√3/3, √3/3, √3/3))")
      val n = normalAt(s, point(sqrt(3)/3, sqrt(3)/3, sqrt(3)/3))
      Then("n = vector(0, 0, 1)")
      assert(n == vector(sqrt(3)/3, sqrt(3)/3, sqrt(3)/3))
    }

    scenario("The normal is a normalized vector") {
      And("s ← sphere()")
      val s = sphere()
      When("n ← normalAt(s, point(√3/3, √3/3, √3/3)")
      val n = normalAt(s, point(sqrt(3)/3, sqrt(3)/3, sqrt(3)/3))
      Then("n = normalize(n)")
      assert(n == normalize(n))
    }

    scenario("Computing the normal on a translated sphere") {
      And("s ← sphere()")
      And("set_transform(s, translation(0, 1, 0))")
      val s = sphere(translation(0, 1, 0))
      When("n ← normalAt(s, point(0, 1.70711, -0.70711))")
      val n = normalAt(s, point(0, 1.70711, -0.70711))
      Then("n = vector(0, 0.70711, -0.70711)")
      assert(n == vector(0, 0.70711, -0.70711))
    }

    scenario("Computing the normal on a transformed sphere") {
      And("s ← sphere()")
      And("m ← scaling(1, 0.5, 1) * rotation_z(π/5)")
      And("set_transform(s, m)")
      val m = scaling(1, 0.5, 1) * rotationZ(π/5)
      val s = sphere(m)
      When("n ← normalAt(s, point(0, √2/2, -√2/2))")
      val n = normalAt(s, point(0, sqrt(2)/2, -sqrt(2)/2))
      Then("n = vector(0, 0.97014, -0.24254)")
      assert(n == vector(0, 0.97014, -0.24254))
    }

    scenario("A sphere has a default material") {
      Given("s ← sphere()")
      val s = sphere()
      When("m ← s.material")
      val m = s.material
      Then("m = material()")
      assert(m == material())
    }

    scenario("A sphere may be assigned a material") {
      Given("s ← sphere()")
      And("m ← material()")
      And("m.ambient ← 1")
      When("s.material ← m")
      Then("s.material = m")
      val m = material().copy(ambient = 1)
      assert(m.ambient == 1d)
      val s = sphere(material = m)
      assert(s.material == m)
    }

    scenario("A helper for producing a sphere with a glassy material") {
      Given("s ← glass_sphere()")
      Then("s.transform = identity_matrix")
      And("s.material.transparency = 1.0")
      And(" s.material.refractive_index = 1.5")

      val s = glassSphere()
      assert(s.transform == identityMatrix)
      assert(s.material.transparency == 1.0)
      assert(s.material.refractiveIndex == 1.5)
    }

    scenario("A sphere has a bounding box") {

      Given("shape ← sphere()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -1, -1)")
      And("box.max = point(1, 1, 1)")

      val box = sphere().bounds
      assert(box.minimum == point(-1, -1, -1))
      assert(box.maximum == point(1, 1, 1))

    }
  }
}