package raytracer.math

import org.scalatest.prop.TableDrivenPropertyChecks._
import raytracer.math.Transforms.{Scaling, Translation}
import raytracer.{BaseSpec, Material}
import raytracer.shapes.Shape

import scala.math.sqrt

class IntersectionsSpec extends BaseSpec {

  feature("Intersections") {

    scenario("An intersection encapsulates t and object") {
      Given("s ← sphere()")
      When("i ← intersection(3.5, s)")
      Then("i.t = 3.5")
      And("i.obj == s")
      val s = sphere()
      val i = Intersection(3.5, s)
      assert(i.t == 3.5)
      assert(i.obj == s)
    }

    scenario("Aggregating intersections") {
      Given("s ← sphere()")
      And(" i1 ← intersection(1, s)")
      And("i2 ← intersection(2, s)")
      When("xs ← intersections(i1, i2)")
      Then("xs.size = 2")
      And("xs(0).t = 1")
      And("xs(1).t = 2")
      val s = sphere()
      val i1 = Intersection(1, s)
      val i2 = Intersection(2, s)
      val xs = intersections(i1, i2)
      assert(xs.size == 2)
      assert(xs(0).t == 1)
      assert(xs(1).t == 2)
    }

    scenario("The hit, when all intersections have positive t") {
      Given("s ← sphere()")
      And(" i1 ← intersection(1, s)")
      And("i2 ← intersection(2, s)")
      When("xs ← intersections(i1, i2)")
      When("i ← hit(xs)")
      Then("i = i1")
      val s = sphere()
      val i1 = Intersection(1, s)
      val i2 = Intersection(2, s)
      val xs = intersections(i1, i2)
      val i =  Intersection.hit(xs)
      assert(i == Some(i1))
    }

    scenario("The hit, when some intersections have negative t") {
      Given("s ← sphere()")
      And(" i1 ← intersection(-1, s)")
      And("i2 ← intersection(1, s)")
      When("xs ← intersections(i1, i2)")
      When("i ← hit(xs)")
      Then("i = i2")
      val s = sphere()
      val i1 = Intersection(-1, s)
      val i2 = Intersection(1, s)
      val xs = intersections(i1, i2)
      val i =  Intersection.hit(xs)
      assert(i == Some(i2))
    }

    scenario("The hit, when all intersections have negative t") {
      Given("s ← sphere()")
      And(" i1 ← intersection(-2, s)")
      And("i2 ← intersection(-1, s)")
      When("xs ← intersections(i1, i2)")
      When("i ← hit(xs)")
      Then("i = i2")
      val s = sphere()
      val i1 = Intersection(-2, s)
      val i2 = Intersection(-1, s)
      val xs = intersections(i1, i2)
      val i =  Intersection.hit(xs)
      assert(i.isEmpty)
    }

    scenario("The hit is always the lowest non-negative intersection") {
      Given("s ← sphere()")
      And("i1 ← intersection(5, s)")
      And("i2 ← intersection(7, s)")
      And("i3 ← intersection(-3, s)")
      And("i4 ← intersection(2, s)")
      When("xs ← intersections(i1, i2, i3, i4)")
      When("i ← hit(xs)")
      Then("i = i4")
      val s = sphere()
      val i1 = Intersection(5, s)
      val i2 = Intersection(7, s)
      val i3 = Intersection(-3, s)
      val i4 = Intersection(2, s)
      val xs = intersections(i1, i2, i3, i4)
      val i =  Intersection.hit(xs)
      assert(i.contains(i4))
    }

    scenario("Precomputing the state of an intersection") {
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("shape ← sphere()")
      And("i ← intersection(4, shape)")
      When("comps ← prepareComputations(i, r)")
      Then("comps.t = i.t")
      And("comps.obj == i.object")
      And("comps.point = point(0, 0, -1)")
      And("comps.eyev = vector(0, 0, -1)")
      And("comps.normalv = vector(0, 0, -1)")
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val shape = sphere()
      val i = Intersection(4, shape)
      val comps = prepareComputations(i, r)
      assert(comps.t == i.t)
      assert(comps.shape == i.obj)
      assert(comps.point == point(0, 0, -1))
      assert(comps.eye == vector(0, 0, -1))
      assert(comps.normal == vector(0, 0, -1))
    }

    scenario("The hit, when an intersection occurs on the outside") {
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("shape ← sphere()")
      And("i ← intersection(4, shape)")
      When("comps ← prepareComputations(i, r)")
      Then("comps.inside = false")
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val shape = sphere()
      val i = Intersection(4, shape)
      val comps = prepareComputations(i, r)
      assert(!comps.inside)
    }

    scenario("The hit, when an intersection occurs on the inside") {
      Given("r ← ray(point(0, 0, 0), vector(0, 0, 1))")
      And("shape ← sphere()")
      And("i ← intersection(1, shape)")
      When("comps ← prepareComputations(i, r)")
      Then("comps.point = point(0, 0, 1)")
      And("comps.eyev = vector(0, 0, -1)")
      And("comps.inside = true")
      // normal would have been (0, 0, 1), but is inverted!
      And("comps.normalv = vector(0, 0, -1)")
      val r = Ray(point(0, 0, 0), vector(0, 0, 1))
      val shape = sphere()
      val i = Intersection(1, shape)
      val comps = prepareComputations(i, r)
      assert(comps.point == point(0, 0, 1))
      assert(comps.eye == vector(0, 0, -1))
      assert(comps.inside)
      assert(comps.normal == vector(0, 0, -1))
    }

    scenario("The hit should offset the point") {
      Given("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And(
        """shape ← sphere() with:
          | transform | translation(0, 0, 1) |""")
      And("i ← intersection(5, shape)")
      When("comps ← prepareComputations(i, r)")
      Then("comps.over_point.z < -EPSILON/2")
      And(" comps.point.z > comps.over_point.z")
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val shape = sphere(Translation(0, 0, 1))
      val i = Intersection(5, shape)
      val comps = prepareComputations(i, r)
      assert(comps.overPoint.z < -EPSILON / 2)
      assert(comps.point.z > comps.overPoint.z)
    }

    scenario("Pre-computing the reflection vector") {
      Given("shape ← plane()")
      And("r ← ray(point(0, 1, -1), vector(0, -√2/2, √2/2))")
      And("i ← intersection(√2, shape)")
      When(" comps ← prepareComputations(i, r)")
      Then("comps.reflectv = vector(0, √2/2, √2/2)")
      val shape = plane()
      val r = Ray(point(0, 1, -1), vector(0, -sqrt(2) / 2, sqrt(2) / 2))
      val i = Intersection(sqrt(2), shape)
      val comps = prepareComputations(i, r)
      assert(comps.reflectV == vector(0, sqrt(2) / 2, sqrt(2) / 2))
    }

    scenario("The under point is offset below the surface") {
      Given(" r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("""shape ← glass_sphere() with:
        |      | transform | translation(0, 0, 1) |""")
      And("i ← intersection(5, shape)")
      And("xs ← intersections(i)")
      When("comps ← prepareComputations(i, r, xs)")
      Then("comps.under_point.z > EPSILON/2")
      And("comps.point.z < comps.under_point.z")

      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val shape = glassSphere(Translation(0, 0, 1))
      val i = Intersection(5, shape)
      val xs = intersections(i)
      val comps = prepareComputations(i, r, xs)
      assert(comps.underPoint.z > EPSILON / 2)
      assert(comps.point.z < comps.underPoint.z)
    }

    scenario("The Schlick approximation under total internal reflection") {
      Given("shape ← glass_sphere()")
      And("r ← ray(point(0, 0, √2/2), vector(0, 1, 0))")
      And("xs ← intersections(-√2/2:shape, √2/2:shape)")
      When("comps ← prepareComputations(xs(1), r, xs)")
      And("reflectance ← schlick(comps)")
      Then("reflectance = 1.0")

      val shape = glassSphere()
      val r = Ray(point(0, 0, sqrt(2)/2), vector(0, 1, 0))
      val xs = intersectionPairs((-sqrt(2)/2, shape), (sqrt(2)/2, shape))
      val comps = prepareComputations(xs(1), r, xs)
      val reflectance = comps.schlick()
      assert(reflectance == 1.0)
    }

    val indexes = Table(
      ("index", "n1", "n2"),
      (0, 1.0, 1.5),
      (1, 1.5, 2.0),
      (2, 2.0, 2.5),
      (3, 2.5, 2.5),
      (4, 2.5, 1.5),
      (5, 1.5, 1.0)
    )

    forAll(indexes) { (index: Int, n1, n2) =>
      scenario(s"Finding n1 and n2 at various intersections: index=$index, n1=$n1, n2=$n2") {
        Given(
          """A ← glass_sphere() with:
            |      | transform                 | scaling(2, 2, 2) |
            |      | material.refractive_index | 1.5              |""")
        And(
          """B ← glass_sphere() with:
            |      | transform                 | translation(0, 0, -0.25) |
            |      | material.refractive_index | 2.0                      |""")
        And(
          """C ← glass_sphere() with:
            |      | transform                 | translation(0, 0, 0.25) |
            |      | material.refractive_index | 2.5                     |""")
        And("r ← ray(point(0, 0, -4), vector(0, 0, 1))")
        And("xs ← intersections(2:A, 2.75:B, 3.25:C, 4.75:B, 5.25:C, 6:A)")
        When("comps ← prepareComputations(xs[<index>], r, xs)")
        Then("comps.n1 = <n1>")
        And("comps.n2 = <n2>")

        val A = Shape()
            .setMaterial(Material.Glass)
            .scale(2, 2, 2)
            .setRefractiveIndex(1.5)
            .sphere

        val B = Shape()
          .setMaterial(Material.Glass)
          .translate(0, 0, -0.25)
          .setRefractiveIndex(2.0)
          .sphere

        val C = Shape()
          .setMaterial(Material.Glass)
          .translate(0, 0, 0.25)
          .setRefractiveIndex(2.5)
          .sphere

        assert(A.material.refractiveIndex == 1.5)
        assert(B.material.refractiveIndex == 2.0)
        assert(C.material.refractiveIndex == 2.5)
        assert(A.transform == Scaling(2, 2, 2).matrix)
        assert(B.transform == Translation(0, 0, -0.25).matrix)
        assert(C.transform == Translation(0, 0, 0.25).matrix)

        val r = Ray(point(0, 0, -4), vector(0, 0, 1))
        val xs = intersectionPairs((2.0, A), (2.75, B), (3.25, C), (4.75, B), (5.25, C), (6.0, A))

        val comps = prepareComputations(xs(index), r, xs)
        assert(comps.n1 == n1)
        assert(comps.n2 == n2)
      }
    }

    scenario("The Schlick approximation with a perpendicular viewing angle") {
      Given("shape ← glass_sphere()")
        And("r ← ray(point(0, 0, 0), vector(0, 1, 0))")
        And("xs ← intersections(-1:shape, 1:shape)")
       When("comps ← prepareComputations(xs(1), r, xs)")
        And("reflectance ← schlick(comps)")
       Then("reflectance = 0.04")

      val shape = glassSphere()
      val r = Ray(point(0, 0, 0), vector(0, 1, 0))
      val xs = intersectionPairs((-1, shape), (1, shape))
      val comps = prepareComputations(xs(1), r, xs)
      val reflectance = comps.schlick()
      assert(reflectance ~= 0.04)
    }

    scenario("The Schlick approximation with small angle and n2 > n1") {
      Given("shape ← glass_sphere()")
        And("r ← ray(point(0, 0.99, -2), vector(0, 0, 1))")
        And("xs ← intersections(1.8589:shape)")
       When("comps ← prepareComputations(xs(0), r, xs)")
        And("reflectance ← schlick(comps)")
       Then("reflectance = 0.48873")

      val shape = glassSphere()
      val r = Ray(point(0, 0.99, -2), vector(0, 0, 1))
      val xs = intersectionPairs((1.8589, shape))
      val comps = prepareComputations(xs(0), r, xs)
      val reflectance = comps.schlick()
      assert(reflectance ~= 0.48873)
    }

    scenario("An intersection can encapsulate `u` and `v`") {
      Given("s ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))")
       When("i ← intersection_with_uv(3.5, s, 0.2, 0.4)")
       Then("i.u = 0.2")
        And("i.v = 0.4")

      val s = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
      val i = Intersection(3.5, s, 0.2, 0.4)
      assert(i.u ~= 0.2)
      assert(i.v ~= 0.4)
    }
  }


}
