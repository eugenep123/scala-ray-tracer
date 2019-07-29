package raytracer

class PlaneSpec extends BaseSpec {
  feature("Planes") {

    scenario("The normal of a plane is constant everywhere") {
      Given("p ← plane()")
      val p = plane()
      When("n1 ← local_normal_at(p, point(0, 0, 0))")
      val n1 = localNormalAt(p, point(0, 0, 0))
      And("n2 ← localNormalAt(p, point(10, 0, -10))")
      val n2 = localNormalAt(p, point(10, 0, -10))
      And("n3 ← localNormalAt(p, point(-5, 0, 150))")
      val n3 = localNormalAt(p, point(-5, 0, 150))
      Then("n1 = vector(0, 1, 0)")
      assert(n1 == vector(0, 1, 0))
      And("n2 = vector(0, 1, 0)")
      assert(n2 == vector(0, 1, 0))
      And("n3 = vector(0, 1, 0)")
      assert(n3 == vector(0, 1, 0))
    }

    scenario("Intersect with a ray parallel to the plane") {
      Given(" p ← plane()")
      val p = plane()
      And("r ← ray(point(0, 10, 0), vector(0, 0, 1))")
      val r = ray(point(0, 10, 0), vector(0, 0, 1))
      When(" xs ← local_intersect(p, r)")
      val xs = localIntersect(p, r)
      Then("xs is empty")
      assert(xs.isEmpty)
    }

    scenario("Intersect with a coplanar ray") {
      Given(" p ← plane()")
      val p = plane()
      And("r ← ray(point(0, 0, 0), vector(0, 0, 1))")
      val r = ray(point(0, 0, 0), vector(0, 0, 1))
      When(" xs ← local_intersect(p, r)")
      val xs = localIntersect(p, r)
      Then("xs is empty")
      assert(xs.isEmpty)
    }

    scenario("A ray intersecting a plane from above") {
      Given(" p ← plane()")
      val p = plane()
      And("r ← ray(point(0, 1, 0), vector(0, -1, 0))")
      val r = ray(point(0, 1, 0), vector(0, -1, 0))
      When(" xs ← local_intersect(p, r)")
      val xs = localIntersect(p, r)
      Then("xs.count = 1")
      assert(xs.size == 1)
      And(" xs[0].t = 1")
      assert(xs.head.t ~= 1)
      And("xs[0].object = p")
      assert(xs.head.obj == p)
    }

    scenario("A ray intersecting a plane from below") {
      Given(" p ← plane()")
      val p = plane()
      And("r ← ray(point(0, -1, 0), vector(0, 1, 0))")
      val r = ray(point(0, -1, 0), vector(0, 1, 0))
      When(" xs ← local_intersect(p, r)")
      val xs = localIntersect(p, r)
      Then("xs.count = 1")
      assert(xs.size == 1)
      And(" xs[0].t = 1")
      assert(xs.head.t ~= 1)
      And("xs[0].object = p")
      assert(xs.head.obj == p)
    }

    scenario("A plane has a bounding box") {
      Given("shape ← plane()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-infinity, 0, -infinity)")
      And("box.max = point(infinity, 0, infinity)")

      val box =  plane().bounds
      assert(box.minimum == point(-INFINITY, 0, -INFINITY))
      assert(box.maximum == point(INFINITY, 0, INFINITY))
    }
  }
}
