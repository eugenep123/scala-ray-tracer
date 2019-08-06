package raytracer.math

import raytracer.BaseSpec

class RaySpec extends BaseSpec {

  feature("Rays") {

    scenario("Creating and querying a ray") {
      val origin = point(1, 2, 3)
      val direction = vector(4, 5, 6)
      val r = ray(origin, direction)
      Given(s"origin ← $origin")
      And(s"direction ← $direction")
      When("r ← ray(origin, direction)")
      Then("r.origin = origin")
      assert(r.origin == origin)
      And("r.direction = direction")
      assert(r.direction == direction)

    }

    scenario("Computing a point from a distance") {
      val r = ray(point(2, 3, 4), vector(1, 0, 0))
      assert(r.toString == "ray(point(2, 3, 4, 1), vector(1, 0, 0, 0))")
      Given(s"r ← $r")
      Then(" position(r, 0) = point(2, 3, 4)")
      assert(position(r, 0) == point(2, 3, 4))
      And("position(r, 1) = point(3, 3, 4)")
      assert(position(r, 1) == point(3, 3, 4))
      And("position(r, -1) = point(1, 3, 4)")
      assert(position(r, -1) == point(1, 3, 4))
      And("position(r, 2.5) = point(4.5, 3, 4)")
      assert(position(r, 2.5) == point(4.5, 3, 4))
    }

    scenario("Translating a ray") {
      val r = ray(point(1, 2, 3), vector(0, 1, 0))
      Given(s"r ← $r")
      val m = translation(3, 4, 5)
      And(s"m ← $m")
      When("r2 ← transform(r, m)")
      val r2 = transform(r, m)
      Then("r2.origin = point(4, 6, 8)")
      assert(r2.origin == point(4, 6, 8))
      And("r2.direction = vector(0, 1, 0)")
      assert(r2.direction == vector(0, 1, 0))
    }

    scenario("Scaling a ray") {
      val r = ray(point(1, 2, 3), vector(0, 1, 0))
      Given(s"r ← $r")
      val m = scaling(2, 3, 4)
      And(s"m ← $m")
      When("r2 ← transform(r, m)")
      val r2 = transform(r, m)
      Then("r2.origin = point(2, 6, 12)")
      assert(r2.origin == point(2, 6, 12))
      And("r2.direction = vector(0, 3, 0)")
      assert(r2.direction == vector(0, 3, 0))
    }
  }

}
