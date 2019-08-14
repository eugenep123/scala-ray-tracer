package raytracer.shapes

import raytracer.BaseSpec
import raytracer.math._

class SmoothTrianglesSpec extends BaseSpec {

  feature("Smooth Triangles") {
    Given("p1 ← point(0, 1, 0)")
      And("p2 ← point(-1, 0, 0)")
      And("p3 ← point(1, 0, 0)")
      And("n1 ← vector(0, 1, 0)")
      And("n2 ← vector(-1, 0, 0)")
      And("n3 ← vector(1, 0, 0)")
     When("tri ← smooth_triangle(p1, p2, p3, n1, n2, n3)")

      val p1 = point(0, 1, 0)
      val p2 = point(-1, 0, 0)
      val p3 = point(1, 0, 0)
      val n1 = vector(0, 1, 0)
      val n2 = vector(-1, 0, 0)
      val n3 = vector(1, 0, 0)
      val tri = Triangle.smooth(p1, p2, p3, n1, n2, n3)
      val data = tri.data.asInstanceOf[TriangleData.Smooth]

    scenario("Constructing a smooth triangle") {
      Then("tri.p1 = p1")
      And("tri.p2 = p2")
      And("tri.p3 = p3")
      And("tri.n1 = n1")
      And("tri.n2 = n2")
      And("tri.n3 = n3")

      assert(tri.p1 == p1)
      assert(tri.p2 == p2)
      assert(tri.p3 == p3)
      assert(data.n1 == n1)
      assert(data.n2 == n2)
      assert(data.n3 == n3)
    }

    scenario("An intersection can encapsulate `u` and `v`") {

    Given("s ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
     When("i ← intersection_with_uv(3.5, s, 0.2, 0.4)")
     Then("i.u = 0.2")
      And("i.v = 0.4")

      val s = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val i = Intersection(3.5, s, 0.2, 0.4)
      assert(i.u == 0.2)
      assert(i.v == 0.4)
    }

    scenario("An intersection with a smooth triangle stores u/v") {
      When("r ← ray(point(-0.2, 0.3, -2), vector(0, 0, 1))")
      And("xs ← local_intersect(tri, r)")
      Then("xs[0].u = 0.45")
      And("xs[0].v = 0.25")

      val r = Ray(point(-0.2, 0.3, -2), vector(0, 0, 1))
      val xs = tri.localIntersect(r)
      assert(xs(0).u ~= 0.45)
      assert(xs(0).v ~= 0.25)
    }

    scenario("A smooth triangle uses u/v to interpolate the normal") {
      When("i ← intersection_with_uv(1, tri, 0.45, 0.25)")
      And("n ← normal_at(tri, point(0, 0, 0), i)")
      Then("n = vector(-0.5547, 0.83205, 0)")

      val i = Intersection(1, tri, 0.45, 0.25)
      val n = tri.normalAt(point(0, 0, 0), i)
      assert(n == vector(-0.5547, 0.83205, 0))
    }

    scenario("Preparing the normal on a smooth triangle") {
      When("i ← intersection_with_uv(1, tri, 0.45, 0.25)")
      And("r ← ray(point(-0.2, 0.3, -2), vector(0, 0, 1))")
      And("xs ← intersections(i)")
      And("comps ← prepare_computations(i, r, xs)")
      Then("comps.normalv = vector(-0.5547, 0.83205, 0)")

      val i = Intersection(1, tri, 0.45, 0.25)
      val r = Ray(point(-0.2, 0.3, -2), vector(0, 0, 1))
      val xs = intersections(i)
      val comps = prepareComputations(i, r, xs)
      assert(comps.normal == vector(-0.5547, 0.83205, 0))
    }
  }
}