package raytracer
package shapes

import scala.math.sqrt

class ShapesSpec extends BaseSpec {

  feature("Abstract Shapes") {

    scenario("The default transformation") {
      Given("s ← test_shape()")
      val s = testShape()
      Then("s.transform = identity_matrix")
      assert(s.transform == identityMatrix)
    }

    scenario("Assigning a transformation") {
      Given("s ← test_shape()")
      When("set_transform(s, translation(2, 3, 4))")
      val s = testShape()
        .setTransform(translation(2, 3, 4))
      assert(s.transform == translation(2, 3, 4).matrix)
    }

    scenario("The default material") {
      Given("s ← test_shape()")
      val s = testShape()
      When("m ← s.material")
      val m = s.material
      Then("m = material()")
      assert(m == material())
    }

    scenario("Assigning a material") {
      Given("s ← test_shape()")
      val s = testShape()
      When("m ← s.material")
      And("m.ambient ← 1")
      val m = material().copy(ambient = 1)
      When("s.material ← m")
      Then("s.material = m")
      assert(s.setMaterial(m).material == m)
    }

    scenario("Intersecting a scaled shape with a ray") {
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given("s ← test_shape()")
      When("set_transform(s, scaling(2, 2, 2))")
      val s = testShape().setTransform(scaling(2, 2, 2))
      And("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("s.saved_ray.origin = point(0, 0, -2.5)")
      assert(s.savedRay.value.origin == point(0, 0, -2.5))
      And("s.saved_ray.direction = vector(0, 0, 0.5)")
      assert(s.savedRay.value.direction == vector(0, 0, 0.5))
    }

    scenario("Intersecting a translated shape with a ray") {
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      Given("s ← test_shape()")
      When("set_transform(s, translation(5, 0, 0))")
      val s = testShape().setTransform(translation(5, 0, 0))
      And("xs ← intersect(s, r)")
      val xs = intersect(s, r)
      Then("s.saved_ray.origin = point(-5, 0, -5)")
      assert(s.savedRay.value.origin == point(-5, 0, -5))
      And("s.saved_ray.direction = vector(0, 0, 1)")
      assert(s.savedRay.value.direction == vector(0, 0, 1))
    }

    scenario("Computing the normal on a translated shape") {
      Given("s ← test_shape()")
      When("set_transform(s, translation(0, 1, 0))")
      val s = testShape().setTransform(translation(0, 1, 0))
      And("n ← normal_at(s, point(0, 1.70711, -0.70711))")
      val n = normalAt(s, point(0, 1.70711, -0.70711))
      Then("n = vector(0, 0.70711, -0.70711)")
      assert(n == vector(0, 0.70711, -0.70711))
    }

    scenario("Computing the normal on a transformed shape") {
      Given("s ← test_shape()")
      And("m ← scaling(1, 0.5, 1) * rotation_z(π/5)")
      When("set_transform(s, m)")
      val m = scaling(1, 0.5, 1) * rotationZ(π/5)
      val s = testShape().setTransform(m)
      And("n ← normal_at(s, point(0, √2/2, -√2/2))")
      val n = normalAt(s, point(0, sqrt(2)/2, -sqrt(2)/2))
      Then("n = vector(0, 0.97014, -0.24254)")
      assert(n == vector(0, 0.97014, -0.24254))
    }

    scenario("A shape has a parent attribute") {
      Given("s ← test_shape()")
      Then("s.parent is nothing")

      val s = testShape()
      assert(s.parent.isEmpty)
    }

    scenario("Converting a point from world to object space") {
      Given("g1 ← group()")
      And("set_transform(g1, rotation_y(π/2))")
      And("g2 ← group()")
      And("set_transform(g2, scaling(2, 2, 2))")
      And("add_child(g1, g2)")
      And("s ← sphere()")
      And("set_transform(s, translation(5, 0, 0))")
      And("add_child(g2, s)")
      When("p ← world_to_object(s, point(-2, 0, -10))")
      Then("p = point(0, 0, -1)")

      val g1 = group()
      setTransform(g1, rotationY(π/2))

      val g2 = group()
      setTransform(g2, scaling(2, 2, 2))
      addChild(g1, g2)

      val s = sphere()
      setTransform(s, translation(5, 0, 0))
      addChild(g2, s)

      val p = worldToObject(s, point(-2, 0, -10))
      assert(p == point(0, 0, -1))
    }

    scenario("Converting a normal from object to world space") {
      Given("g1 ← group()")
      And("set_transform(g1, rotation_y(π/2))")
      And("g2 ← group()")
      And("set_transform(g2, scaling(1, 2, 3))")
      And("add_child(g1, g2)")
      And("s ← sphere()")
      And("set_transform(s, translation(5, 0, 0))")
      And("add_child(g2, s)")
      When("n ← normal_to_world(s, vector(√3/3, √3/3, √3/3))")
      Then("n = vector(0.2857, 0.4286, -0.8571)")

      val g1 = group()
      setTransform(g1, rotationY(π/2))

      val g2 = group()
      setTransform(g2, scaling(1, 2, 3))
      addChild(g1, g2)

      val s = sphere()
      setTransform(s, translation(5, 0, 0))
      addChild(g2, s)

      val n = normalToWorld(s, vector(sqrt(3)/3, sqrt(3)/3, sqrt(3)/3))
      assert(n == vector(0.2857, 0.4286, -0.8571))
    }

    scenario("Finding the normal on a child object") {
      Given("g1 ← group()")
      And("set_transform(g1, rotation_y(π/2))")
      And("g2 ← group()")
      And("set_transform(g2, scaling(1, 2, 3))")
      And("add_child(g1, g2)")
      And("s ← sphere()")
      And("set_transform(s, translation(5, 0, 0))")
      And("add_child(g2, s)")
      When("n ← normal_at(s, point(1.7321, 1.1547, -5.5774))")
      Then("n = vector(0.2857, 0.4286, -0.8571)")

      val g1 = group()
      setTransform(g1, rotationY(π/2))

      val g2 = group()
      setTransform(g2, scaling(1, 2, 3))
      addChild(g1, g2)

      val s = sphere()
      setTransform(s, translation(5, 0, 0))
      addChild(g2, s)

      val n = normalAt(s, point(1.7321, 1.1547, -5.5774))
      assert(n == vector(0.2857, 0.4286, -0.8571))
    }


    scenario("Test shape has (arbitrary) bounds") {
      Given("shape ← test_shape()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -1, -1)")
      And("box.max = point(1, 1, 1)")

      val box = testShape().bounds
      assert(box.minimum == point(-1, -1, -1))
      assert(box.maximum == point(1, 1, 1))
    }
  }


}
/*



Scenario: Querying a shape's bounding box in its parent's space
    Given shape ← sphere()
  And set_transform(shape, translation(1, -3, 5) * scaling(0.5, 2, 4))
  When box ← parent_space_bounds_of(shape)
  Then box.min = point(0.5, -5, 1)
  And box.max = point(1.5, -1, 9)

Scenario: Subdividing a primitive does nothing
  Given shape ← sphere()
  When divide(shape, 1)
  Then shape is a sphere
 */
