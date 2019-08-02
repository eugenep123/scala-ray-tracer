package raytracer
package shapes

import org.scalatest.prop.TableDrivenPropertyChecks._

class CylindersSpec extends BaseSpec {

  feature("Cylinders: A ray misses a cylinder") {

    val values = Table(
      ( "origin"        , "direction"     ),
      ( point(1, 0, 0)  , vector(0, 1, 0) ),
      ( point(0, 0, 0)  , vector(0, 1, 0) ),
      ( point(0, 0, -5) , vector(1, 1, 1) )
    )

    forAll(values) { (origin: Point3D, d: Vector3D) =>
      scenario(s"A ray misses a cylinder: $origin - $d") {
        Given("cyl ← cylinder()")
        And(s"direction ← normalize($d)")
        And(s"r ← ray($origin, direction)")
        When("xs ← local_intersect(cyl, r)")
        Then("xs.count = 0")
        val cyl = cylinder()
        val direction = normalize(d)
        val r = ray(origin, direction)
        val xs = localIntersect(cyl, r)
        assert(xs.size == 0)
      }
    }
  }

  feature("Cylinders: A ray strikes a cylinder") {

    val values = Table(
      ( "origin"          , "direction"       , "t0"    , "t1"    ),
      ( point(1, 0, -5)   , vector(0, 0, 1)   , 5d       , 5d       ),
      ( point(0, 0, -5)   , vector(0, 0, 1)   , 4d       , 6d       ),
      ( point(0.5, 0, -5) , vector(0.1, 1, 1) , 6.80798 , 7.08872 )
    )
    forAll(values) { (origin: Point3D, direction: Vector3D, t0: Double, t1: Double) =>

      scenario(s"A ray strikes a cylinder: $origin - $direction ") {
        Given("cyl ← cylinder()")
        And(s"direction ← normalize($direction)")
        And("r ← ray(<origin>, direction)")
        When("xs ← local_intersect(cyl, r)")
        Then("xs.count = 2")
        And("xs[0].t = <t0>")
        And("xs[1].t = <t1>")

        val cyl = cylinder()
        val directionNormal = normalize(direction).normalize
        val r = ray(origin, directionNormal)
        val xs = localIntersect(cyl, r)
        assert(xs.size == 2)
        assert(xs(0).t ~= t0)
        assert(xs(1).t ~= t1)
      }
    }
  }

  feature("Cylinders: Normal vector on a cylinder") {
    val values = Table(
      ( "point"         , "normal"         ),
      ( point(1, 0, 0)  , vector(1, 0, 0)  ),
      ( point(0, 5, -1) , vector(0, 0, -1) ),
      ( point(0, -2, 1) , vector(0, 0, 1)  ),
      ( point(-1, 1, 0) , vector(-1, 0, 0) )
    )

    forAll(values) { (point, normal) =>

      scenario(s"Normal vector on a cylinder: $point") {
        Given("cyl ← cylinder()")
        When(s"n ← local_normal_at(cyl, $point)")
        Then (s"n = $normal")
        val cyl = cylinder()
        val n = localNormalAt(cyl, point)
        assert(n == normal)
      }
    }
  }

  feature("Cylinders") {

    scenario("The default minimum and maximum for a cylinder") {
      Given("cyl ← cylinder()")
      Then("cyl.minimum = -infinity")
      And("cyl.maximum = infinity")

      val cyl = cylinder()
      assert(cyl.minimum == -INFINITY)
      assert(cyl.maximum == INFINITY)
    }

    scenario("The default closed value for a cylinder") {
      Given("cyl ← cylinder()")
      Then("cyl.closed = false")
      val cyl = cylinder()
      assert(!cyl.closed)
    }

    scenario("An unbounded cylinder has a bounding box") {
      Given("shape ← cylinder()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -infinity, -1)")
      And("box.max = point(1, infinity, 1)")
      val box = cylinder().bounds
      assert(box.minimum == point(-1, -INFINITY, -1))
      assert(box.maximum == point(1, INFINITY, 1))
    }

    scenario("A bounded cylinder has a bounding box") {

      Given("shape ← cylinder()")
      And("shape.minimum ← -5")
      And("shape.maximum ← 3")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -5, -1)")
      And("box.max = point(1, 3, 1)")

      val box = cylinder().setMinimum(-5).setMaximum(3).bounds
      assert(box.minimum == point(-1, -5, -1))
      assert(box.maximum == point(1, 3, 1))
    }
  }

  feature("Cylinders: Intersecting a constrained cylinder") {

    val values = Table(
      ( "i", "point"           , "direction"       , "count" ),
      ( 1  , point(0, 1.5, 0)  , vector(0.1, 1, 0) , 0       ),
      ( 2  , point(0, 3, -5)   , vector(0, 0, 1)   , 0       ),
      ( 3  , point(0, 0, -5)   , vector(0, 0, 1)   , 0       ),
      ( 4  , point(0, 2, -5)   , vector(0, 0, 1)   , 0       ),
      ( 5  , point(0, 1, -5)   , vector(0, 0, 1)   , 0       ),
      ( 6  , point(0, 1.5, -2) , vector(0, 0, 1)   , 2       )
    )

    forAll(values) { case (i, point, direction, count) =>

      scenario(s"Intersecting a constrained cylinder: $i") {
        Given("cyl ← cylinder()")
        And("cyl.minimum ← 1")
        And("cyl.maximum ← 2")
        And(s"direction ← normalize($direction)")
        And(s"r ← ray($point, direction)")
        When("xs ← local_intersect(cyl, r)")
        Then(s"xs.count = $count")

        val cyl = cylinder()
            .setMinimum(1)
            .setMaximum(2)
        val directionNormal = normalize(direction)
        val r = ray(point, directionNormal)
        val xs = localIntersect(cyl, r)
        assert(xs.size == count)

      }
    }
  }

  feature("Cylinders: Intersecting the caps of a closed cylinder") {

    val values = Table(
      ( "i" , "point"          , "direction"      , "count" ),
      (  1  , point(0, 3, 0)   , vector(0, -1, 0) , 2       ),
      (  2  , point(0, 3, -2)  , vector(0, -1, 2) , 2       ),
      (  3  , point(0, 4, -2)  , vector(0, -1, 1) , 2       ), // corner case
      (  4  , point(0, 0, -2)  , vector(0, 1, 2)  , 2       ),
      (  5  , point(0, -1, -2) , vector(0, 1, 1)  , 2       ) // corner case
    )

    forAll(values) { case (i: Int, point: Point3D, direction: Vector3D, count: Int) =>
      scenario(s"Intersecting the caps of a closed cylinder: $i") {
        Given("cyl ← cylinder()")
        And("cyl.minimum ← 1")
        And("cyl.maximum ← 2")
        And("cyl.closed ← true")
        And(s"direction ← normalize($direction)")
        And(s"r ← ray($point, direction)")
        When("xs ← local_intersect(cyl, r)")
        Then("xs.count = <count>")

        val cyl = cylinder()
          .setMinimum(1)
          .setMaximum(2)
          .setClosed(true)
        val directionNormal = normalize(direction)
        val r = ray(point, directionNormal)
        assert(cyl.closed)
        val xs = localIntersect(cyl, r)
        assert(xs.size == count)
      }
    }
  }

  feature("Cylinders: The normal vector on a cylinder's end caps") {

    val values = Table(
      ( "point"          , "normal"         ),
      ( point(0, 1, 0)   , vector(0, -1, 0) ),
      ( point(0.5, 1, 0) , vector(0, -1, 0) ),
      ( point(0, 1, 0.5) , vector(0, -1, 0) ),
      ( point(0, 2, 0)   , vector(0, 1, 0)  ),
      ( point(0.5, 2, 0) , vector(0, 1, 0)  ),
      ( point(0, 2, 0.5) , vector(0, 1, 0)  )
    )

    forAll(values) { case (point: Point3D, normal: Vector3D) =>
      scenario(s"Intersecting the caps of a closed cylinder: $point, $normal") {
        Given("cyl ← cylinder()")
        And("cyl.minimum ← 1")
        And("cyl.maximum ← 2")
        And("cyl.closed ← true")
        When(s"n ← local_normal_at(cyl, $point)")
        Then("n = <normal>")

        val cyl = cylinder()
          .setMinimum(1)
          .setMaximum(2)
          .setClosed(true)
        val n = localNormalAt(cyl, point)
        assert(n == normal)
      }
    }
  }
}

/*




 */