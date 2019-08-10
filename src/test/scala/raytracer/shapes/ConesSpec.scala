package raytracer
package shapes

import org.scalatest.prop.TableDrivenPropertyChecks._
import raytracer.math._

import scala.math.sqrt

class ConesSpec extends BaseSpec {

  feature("Cones: Intersecting a cone with a ray") {
    val values = Table(
      ("origin", "direction", "t0", "t1"),
      (point(0, 0, -5), vector(0, 0, 1), 5.0, 5.0),
      (point(0, 0, -5), vector(1, 1, 1), 8.66025, 8.66025),
      (point(1, 1, -5), vector(-0.5, -1, 1), 4.55006, 49.44994)
    )

    forAll(values) { (origin: Point3D, direction: Vector3D, t0: Double, t1: Double) =>
      scenario(s"Intersecting a cone with a ray: $origin, $direction") {
        Given("shape ← cone()")
        And(s"direction ← normalize($direction)")
        And(s"r ← ray($origin, direction)")
        When("xs ← local_intersect(shape, r)")
        Then("xs.count = 2")
        And("xs[0].t = <t0>")
        And("xs[1].t = <t1>")

        val shape = cone()
        val direction2 = direction.normalize
        val r = Ray(origin, direction2)
        val xs = shape.localIntersect(r)
        assert(xs.size == 2)
        assert(xs(0).t ~= t0)
        assert(xs(1).t ~= t1)
      }
    }
  }

  feature("Cones") {
    scenario("Intersecting a cone with a ray parallel to one of its halves") {
      Given("shape ← cone()")
      And("direction ← normalize(vector(0, 1, 1))")
      And("r ← ray(point(0, 0, -1), direction)")
      When("xs ← local_intersect(shape, r)")
      Then("xs.count = 1")
      And("xs[0].t = 0.35355")

      val shape = cone()
      val direction = vector(0, 1, 1).normalize
      val r = Ray(point(0, 0, -1), direction)
      val xs = shape.localIntersect(r)
      assert(xs.size == 1)
      assert(xs(0).t ~= 0.35355)
    }

    scenario("creating cones") {
      val shape = cone(-0.5, 0.5, true)
      assert(shape.minimum == -0.5)
      assert(shape.maximum == 0.5)
      assert(shape.closed)
    }

    scenario("An unbounded cone has a bounding box") {
      Given("shape ← cone()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-infinity, -infinity, -infinity)")
      And("box.max = point(infinity, infinity, infinity)")

      val box = cone().bounds
      assert(box.minimum == point(-INFINITY, -INFINITY, -INFINITY))
      assert(box.maximum == point(INFINITY, INFINITY, INFINITY))
    }
    scenario("A bounded cone has a bounding box") {

      Given("shape ← cone()")
      And("shape.minimum ← -5")
      And("shape.maximum ← 3")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-5, -5, -5)")
      And("box.max = point(5, 3, 5)")
      val box = cone(-5, 3).bounds
      assert(box.minimum == point(-5, -5, -5))
      assert(box.maximum == point(5, 3, 5))
    }

  }

  feature("Cones: Intersecting a cone's end caps") {
    val values = Table(
      ("origin", "direction", "count"),
      (point(0, 0, -5), vector(0, 1, 0), 0),
      (point(0, 0, -0.25), vector(0, 1, 1), 2),
      (point(0, 0, -0.25), vector(0, 1, 0), 4)
    )

    forAll(values) { (origin: Point3D, direction: Vector3D, count: Int) =>

      scenario(s"Intersecting a cone's end caps: $origin, $direction") {
        Given("shape ← cone()")
        And("shape.minimum ← -0.5")
        And("shape.maximum ← 0.5")
        And("shape.closed ← true")
        And(s"direction ← normalize($direction)")
        And("r ← ray(<origin>, direction)")
        When("xs ← local_intersect(shape, r)")
        Then("xs.count = <count>")

        val shape = cone(-0.5, 0.5, true)
        val r = Ray(origin, direction.normalize)
        val xs = shape.localIntersect(r)
        assert(xs.size == count)
      }
    }
  }

  feature("Cones: Computing the normal vector on a cone") {
    val values = Table(
      ("point",           "normal"                ),
      (point(0, 0, 0),    vector(0, 0, 0)         ),
      (point(1, 1, 1),    vector(1, -sqrt(2.0), 1)  ),
      (point(-1, -1, 0),  vector(-1, 1, 0)        )
    )

    forAll(values) { (point: Point3D, normal: Vector3D) =>

      scenario(s"Computing the normal vector on a cone: $point, $normal") {
        Given("shape ← cone()")
        When(s"n ← local_normal_at(shape, $point)")
        Then(s"n = $normal")
        val shape = cone()
        val n = localNormalAt(shape, point)
        assert(n == normal)
      }
    }
  }
}
