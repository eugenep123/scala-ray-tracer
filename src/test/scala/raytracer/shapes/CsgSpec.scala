package raytracer.shapes

import raytracer.BaseSpec
import org.scalatest.prop.TableDrivenPropertyChecks._
import raytracer.math.Ray
import raytracer.math.Transforms.Translation

class CsgSpec extends BaseSpec {

  feature("Constructive Solid Geometry (CSG)") {

    scenario("CSG is created with an operation and two shapes") {
      Given("s1 ← sphere()")
      And("s2 ← cube()")
      When("c ← csg('union', s1, s2)")
      Then("c.operation = 'union'")
      And("c.left = s1")
      And("c.right = s2")
      And("s1.parent = c")
      And("s2.parent = c")

      val s1 = sphere()
      val s2 = cube()
      val c = csg(Csg.Union, s1, s2)
      assert(c.operation == Csg.Union)
      assert(c.left == s1)
      assert(c.right == s2)
      assert(s1.parent == Some(c))
      assert(s2.parent == Some(c))
    }

    import Csg._
    val table = Table(
      ("op"           , "lhit", "inl" , "inr" , "result"  ),
      ( Union         , true  , true  , true  , false     ),
      ( Union         , true  , true  , false , true      ),
      ( Union         , true  , false , true  , false     ),
      ( Union         , true  , false , false , true      ),
      ( Union         , false , true  , true  , false     ),
      ( Union         , false , true  , false , false     ),
      ( Union         , false , false , true  , true      ),
      ( Union         , false , false , false , true      ),
      //  # append after the union examples...
      ( Intersection  , true  , true  , true  , true      ),
      ( Intersection  , true  , true  , false , false     ),
      ( Intersection  , true  , false , true  , true      ),
      ( Intersection  , true  , false , false , false     ),
      ( Intersection  , false , true  , true  , true      ),
      ( Intersection  , false , true  , false , true      ),
      ( Intersection  , false , false , true  , false     ),
      ( Intersection  , false , false , false , false     ),
    // append after the intersection examples...
      ( Difference    , true  , true  , true  , false     ),
      ( Difference    , true  , true  , false , true      ),
      ( Difference    , true  , false , true  , false     ),
      ( Difference    , true  , false , false , true      ),
      ( Difference    , false , true  , true  , true      ),
      ( Difference    , false , true  , false , true      ),
      ( Difference    , false , false , true  , false     ),
      ( Difference    , false , false , false , false     )
    )

    forAll(table) { (op, lhit, inl, inr, expected) =>
      scenario(s"Evaluating the rule for a CSG operation: $op, $lhit, $inl, $inr") {
        When(s"result ← intersection_allowed($op, $lhit, $inl, $inr)")
        Then(s"result = $expected")
        val result =  op.allowed(lhit, inl, inr)
        assert(result == expected)
      }
    }

    val filter = Table(
      ( "operation"  , "x0" , "x1"),
      ( Union        , 0    , 3  ),
      ( Intersection , 1    , 2  ),
      ( Difference   , 0    , 1  )
    )

    forAll(filter) { (operation, x0, x1) =>

      scenario(s"Filtering a list of intersections: $operation, $x0, $x0") {
        Given("s1 ← sphere()")
        And("s2 ← cube()")
        And(s"c ← csg($operation, s1, s2)")
        And("xs ← intersections(1:s1, 2:s2, 3:s1, 4:s2)")
        When("result ← filter_intersections(c, xs)")
        Then("result.count = 2")
        And(s"result[0] = xs[$x0]")
        And(s"result[1] = xs[$x1]")

        val s1 = sphere()
        val s2 = cube()
        val c = csg(operation, s1, s2)
        val xs = intersectionPairs((1, s1), (2, s2), (3, s1), (4, s2))
        val result = c.filterIntersections(xs)
        assert(result.size == 2)
        assert(result(0) == xs(x0))
        assert(result(1) == xs(x1))
      }
    }

    scenario("A ray misses a CSG object") {
      Given("c ← csg(union, sphere(), cube())")
        And("r ← ray(point(0, 2, -5), vector(0, 0, 1))")
       When("xs ← local_intersect(c, r)")
       Then("xs is empty")

      val c = csg(Union, sphere(), cube())
      val r = Ray(point(0, 2, -5), vector(0, 0, 1))
      val xs = c.localIntersect(r)
      assert(xs.isEmpty)
    }

    scenario("A ray hits a CSG object") {
      Given("s1 ← sphere()")
      And("s2 ← sphere()")
      And("set_transform(s2, translation(0, 0, 0.5))")
      And("c ← csg(union, s1, s2)")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      When("xs ← local_intersect(c, r)")
      Then("xs.count = 2")
      And("xs[0].t = 4")
      And("xs[0].object = s1")
      And("xs[1].t = 6.5")
      And("xs[1].object = s2")

      val s1 = sphere()
      val s2 = sphere(Translation(0, 0, 0.5))
      val c = csg(Union, s1, s2)
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = c.localIntersect(r)
      assert(xs.size == 2)
      assert(xs(0).t == 4)
      assert(xs(0).obj == s1)
      assert(xs(1).t == 6.5)
      assert(xs(1).obj == s2)
    }

    scenario("A CSG shape has a bounding box that contains its children") {
      Given("left ← sphere()")
      And("""right ← sphere() with:
            |      | transform | translation(2, 3, 4) |""".stripMargin)
      And("shape ← csg(difference, left, right)")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -1, -1)")
      And("box.max = point(3, 4, 5)")

      val left = sphere()
      val right = sphere(Translation(2, 3, 4))
      val shape = csg(Difference, left, right)
      val box = shape.bounds
      assert(box.minimum == point(-1, -1, -1))
      assert(box.maximum == point(3, 4, 5))
    }

    scenario("Intersecting ray+csg doesn't test children if box is missed") {
      Given("left ← test_shape()")
      And("right ← test_shape()")
      And("shape ← csg(difference, left, right)")
      And("r ← ray(point(0, 0, -5), vector(0, 1, 0))")
      When("xs ← intersect(shape, r)")
      Then("left.saved_ray is unset")
      And("right.saved_ray is unset")

      val left = testShape()
      val right = testShape()
      val shape = csg(Difference, left, right)
      val r = Ray(point(0, 0, -5), vector(0, 1, 0))
      val xs = shape.intersect(r)
      assert(left.savedRay.isEmpty)
      assert(right.savedRay.isEmpty)
    }

    scenario("Intersecting ray+csg tests children if box is hit") {
      Given("left ← test_shape()")
      And("right ← test_shape()")
      And("shape ← csg(difference, left, right)")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      When("xs ← intersect(shape, r)")
      Then("left.saved_ray is set")
      And("right.saved_ray is set")

      val left = testShape()
      val right = testShape()
      val shape = csg(Difference, left, right)
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = shape.intersect(r)
      assert(left.savedRay.isDefined)
      assert(right.savedRay.isDefined)
    }

    scenario("Subdividing a CSG shape subdivides its children") {
      Given("""s1 ← sphere() with:
              |      | transform | translation(-1.5, 0, 0) |""".stripMargin)
      And("""s2 ← sphere() with:
              |      | transform | translation(1.5, 0, 0) |""".stripMargin)
      And("left ← group() of [s1, s2]")
      And("""s3 ← sphere() with:
              |      | transform | translation(0, 0, -1.5) |""".stripMargin)
      And("""s4 ← sphere() with:
              |      | transform | translation(0, 0, 1.5) |""".stripMargin)
      And("right ← group() of [s3, s4]")
      And("shape ← csg(difference, left, right)")
      When("divide(shape, 1)")
      Then("left[0] is a group of [s1]")
      And("left[1] is a group of [s2]")
      And("right[0] is a group of [s3]")
      And("right[1] is a group of [s4]")

      val s1 = sphere(Translation(-1.5, 0, 0))
      val s2 = sphere(Translation(1.5, 0, 0))
      val left = group().addChildren(Seq(s1, s2))

      val s3 = sphere(Translation(0, 0, -1.5))
      val s4 = sphere(Translation(0, 0, 1.5))
      val right = group().addChildren(Seq(s3, s4))
      val shape = csg(Difference, left, right)

      shape.divide(1)
      assert(left.children(0).asInstanceOf[Group].children == Seq(s1))
      assert(left.children(1).asInstanceOf[Group].children == Seq(s2))
      assert(right.children(0).asInstanceOf[Group].children == Seq(s3))
      assert(right.children(1).asInstanceOf[Group].children == Seq(s4))
    }
  }
}
