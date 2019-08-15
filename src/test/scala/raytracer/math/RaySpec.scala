package raytracer.math

import raytracer.BaseSpec
import raytracer.math.Transforms.{Scaling, Translation}

class RaySpec extends BaseSpec {

  feature("Rays") {

    scenario("Creating and querying a ray") {
      Given("origin ← point(1, 2, 3)")
      And("direction ← vector(4, 5, 6)")
      When("r ← ray(origin, direction)")
      Then("r.origin = origin")
      And("r.direction = direction")
      val origin = point(1, 2, 3)
      val direction = vector(4, 5, 6)
      val r = Ray(origin, direction)
      assert(r.origin == origin)
      assert(r.direction == direction)
    }

    scenario("Computing a point from a distance") {
      Given("r ← ray(point(2, 3, 4, 1), vector(1, 0, 0, 0))")
      Then(" position(r, 0) = point(2, 3, 4)")
      And("position(r, 1) = point(3, 3, 4)")
      And("position(r, -1) = point(1, 3, 4)")
      And("position(r, 2.5) = point(4.5, 3, 4)")
      val r = Ray(point(2, 3, 4), vector(1, 0, 0))
      assert(r.toString == "ray(point(2, 3, 4, 1), vector(1, 0, 0, 0))")
      assert(r.position(0) == point(2, 3, 4))
      assert(r.position(1) == point(3, 3, 4))
      assert(r.position(-1) == point(1, 3, 4))
      assert(r.position(2.5) == point(4.5, 3, 4))
    }

    scenario("Translating a ray") {
      Given("r ← ray(point(1, 2, 3), vector(0, 1, 0))")
      And("m ← translation(3, 4, 5)")
      When("r2 ← transform(r, m)")
      Then("r2.origin = point(4, 6, 8)")
      And("r2.direction = vector(0, 1, 0)")
      val r = Ray(point(1, 2, 3), vector(0, 1, 0))
      val m = Translation(3, 4, 5)
      val r2 = r.transform(m)
      assert(r2.origin == point(4, 6, 8))
      assert(r2.direction == vector(0, 1, 0))
    }

    scenario("Scaling a ray") {
      Given("r ← ray(point(1, 2, 3), vector(0, 1, 0))")
      And("m ← scaling(2, 3, 4)")
      When("r2 ← transform(r, m)")
      Then("r2.origin = point(2, 6, 12)")
      And("r2.direction = vector(0, 3, 0)")
      val r = Ray(point(1, 2, 3), vector(0, 1, 0))
      val m = Scaling(2, 3, 4)
      val r2 = r.transform(m)
      assert(r2.origin == point(2, 6, 12))
      assert(r2.direction == vector(0, 3, 0))
    }
  }

}
