package raytracer
package shapes

import raytracer.math.Ray

class TriangleSpec extends BaseSpec {

  feature("Triangles") {

    scenario("Constructing a triangle") {
      Given("p1 ← point(0, 1, 0)")
      And("p2 ← point(-1, 0, 0)")
      And("p3 ← point(1, 0, 0)")
      And("t ← triangle(p1, p2, p3)")
      Then("t.p1 = p1")
      And("t.p2 = p2")
      And("t.p3 = p3")
      And("t.e1 = vector(-1, -1, 0)")
      And("t.e2 = vector(1, -1, 0)")
      And("t.normal = vector(0, 0, -1)")
      val p1 = point(0, 1, 0)
      val p2 = point(-1, 0, 0)
      val p3 = point(1, 0, 0)
      val t = triangle(p1, p2, p3).data.asInstanceOf[TriangleData.Default]
      assert(t.p1 == p1)
      assert(t.p2 == p2)
      assert(t.p3 == p3)

      assert(t.e1 == vector(-1, -1, 0))
      assert(t.e2 == vector(1, -1, 0))
      assert(t.normal == vector(0, 0, -1))
    }

    scenario("Finding the normal on a triangle") {
      Given(" t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      When("n1 ← local_normal_at(t, point(0, 0.5, 0))")
      And("n2 ← local_normal_at(t, point(-0.5, 0.75, 0))")
      And("n3 ← local_normal_at(t, point(0.5, 0.25, 0))")
      Then("n1 = t.normal")
      And("n2 = t.normal")
      And("n3 = t.normal")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val data = t.data.asInstanceOf[TriangleData.Default]
      val n1 = localNormalAt(t, point(0, 0.5, 0))
      val n2 = localNormalAt(t, point(-0.5, 0.75, 0))
      val n3 = localNormalAt(t, point(0.5, 0.25, 0))
      assert(n1 == data.normal)
      assert(n2 == data.normal)
      assert(n3 == data.normal)
    }

    scenario("Intersecting a ray parallel to the triangle") {
      Given("t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      And("r ← ray(point(0, -1, -2), vector(0, 1, 0))")
      When("xs ← local_intersect(t, r)")
      Then("xs is empty")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val r = Ray(point(0, -1, -2), vector(0, 1, 0))
      val xs = t.localIntersect(r)
      assert(xs.isEmpty)
    }

    scenario("A ray misses the p1-p3 edge") {
      Given("t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      And(" r ← ray(point(1, 1, -2), vector(0, 0, 1))")
      When("xs ← local_intersect(t, r)")
      Then("xs is empty")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val r = Ray(point(1, 1, -2), vector(0, 0, 1))
      val xs = t.localIntersect(r)
      assert(xs.isEmpty)
    }

    scenario("A ray misses the p1-p2 edge") {
      Given("t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      And("r ← ray(point(-1, 1, -2), vector(0, 0, 1))")
      When("xs ← local_intersect(t, r)")
      Then("xs is empty")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val r = Ray(point(-1, 1, -2), vector(0, 0, 1))
      val xs = t.localIntersect(r)
      assert(xs.isEmpty)
    }


    scenario("A ray misses the p2-p3 edge") {
      Given("t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      And("r ← ray(point(0, -1, -2), vector(0, 0, 1))")
      When("xs ← local_intersect(t, r)")
      Then("xs is empty")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val r = Ray(point(0, -1, -2), vector(0, 0, 1))
      val xs = t.localIntersect(r)
      assert(xs.isEmpty)
    }

    scenario("A ray strikes a triangle") {
      Given("t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
      And("r ← ray(point(0, 0.5, -2), vector(0, 0, 1))")
      When("xs ← local_intersect(t, r)")
      Then("xs.count = 1")
      And("xs[0].t = 2")
      val t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val r = Ray(point(0, 0.5, -2), vector(0, 0, 1))
      val xs = t.localIntersect(r)
      assert(xs.size == 1)
      assert(xs.head.t == 2)
    }

    scenario("A triangle has a bounding box") {
      Given("p1 ← point(-3, 7, 2)")
        And("p2 ← point(6, 2, -4)")
        And("p3 ← point(2, -1, -1)")
        And("shape ← triangle(p1, p2, p3)")
       When("box ← bounds_of(shape)")
       Then("box.min = point(-3, -1, -4)")
        And("box.max = point(6, 7, 2)")

      val p1 = point(-3, 7, 2)
      val p2 = point(6, 2, -4)
      val p3 = point(2, -1, -1)
      val shape = triangle(p1, p2, p3)
      val box = shape.bounds
      assert(box.minimum == point(-3, -1, -4))
      assert(box.maximum == point(6, 7, 2))
    }
  }
}